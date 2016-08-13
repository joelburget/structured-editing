// @flow
import { List } from 'immutable';
import {
  convertFromRaw,
  // convertToRaw,
  CompositeDecorator,
  ContentBlock,
  ContentState,
  Editor,
  EditorState,
  Entity,
  getDefaultKeyBinding,
  KeyBindingUtil,
} from 'draft-js';
import React from 'react';

import type {
  RawDraftContentBlock,
} from 'draft-js';

import {
  intBoolIsTemplated,
  intBoolIsLang,
  intBoolIsOperate,
  initSelectSyntax,
  selectionInfo,
} from './purescript/output/Lang/index.js';

import {
  listAllHoles,
  listAllConflicts,
  genContentState,
  genDisplayContentState,
  setEndpoints,
  operate,
} from './purescript/output/Interface/index.js';

const {hasCommandModifier} = KeyBindingUtil;

const entityTypes = {
  leaf: {
    type: 'TOKEN',
    mutability: 'MUTABLE',
    data: {type: 'number'},
  },
  hole: {
    type: 'TOKEN',
    mutability: 'MUTABLE',
    data: {type: 'hole'},
  },
  internal: {
    type: 'TOKEN',
    mutability: 'MUTABLE',
    data: {type: 'plus'},
  },
  conflict: {
    type: 'TOKEN',
    mutability: 'MUTABLE',
    data: {type: 'conflict'},
  },
};

const components = {
  plus: ({children}) => {
    // assert React.Children.count(children) === 2
    // const [l, r] = children;
    return <span>{children}</span>;
  },
  number: ({children}) => {
    return <span style={styles.number}>{children}</span>
  },
  hole: ({children}) => (
    // TODO
    // {React.Children.only(children)}
    <span style={styles.hole}>{children}</span>
  ),
  conflict: ({children}) => (
      <span style={styles.conflict}>{children}</span>
  ),
};

const DataDecorator = {
  getDecorations(block: ContentBlock): List<?string> {
    const length = block.getLength();
    const arr = [];
    for (let i = 0; i < length; i ++) {
      const key = block.getEntityAt(i);
      arr.push(key);
    }

    return List(arr);
  },

  getComponentForKey(key: string): Function {
    const entity = Entity.get(key);
    const tyName = entity.data.type;
    return components[tyName];
  },

  getPropsForKey(key: string): ?Object {
    return null;
  },
};


// The only operation we need to catch and handle specially is backspace.
// * Regular characters are handled by `handleBeforeInput`
// * Key combinations are handled by `handleKeyCommand`
function specialKeyBindings(e: SyntheticKeyboardEvent): string {
  if (!hasCommandModifier(e)) {
    if (e.key === 'Backspace') {
      return 'structurededitor-backspace';
    } // else if (e.key === 'Tab'
  }

  return getDefaultKeyBinding(e);
}


export class StructuredEditor extends React.Component {
  constructor(props) {
    super(props);

    this.handleKeyCommand = command => this._handleKeyCommand(command);
    this.handleBeforeInput = chars => this._handleBeforeInput(chars);
    this.handleTab = e => this._handleTab(e);
    this.handleRawChange = editorState => this._handleRawChange(editorState);
    this.handleResolve = (loc, command) => this._handleResolve(loc, command);
    this.focus = () => this.editor.focus();
  }

  _handleBeforeInput(chars: string): boolean {
    this.props.onChange({
      tag: 'typing',
      value: chars,
    });
    return true;
  }

  _handleTab(e) {
    if (e.type === 'keydown' && e.key === 'Tab') {
      this.props.onChange({tag: 'tab', shift: e.shiftKey});
      e.preventDefault();
    }
  }

  _handleKeyCommand(command: string): boolean {
    switch (command) {
      case 'structurededitor-backspace':
        this.props.onChange({tag: 'backspace'});
        return true;

      case 'undo':
      case 'redo':
        console.log(command + ' not yet implemented!');
        return true;

      default:
        return false;
    }
  }

  _handleResolve(loc, tag: string) {
    this.props.onChange({tag, loc});
  }

  // "executed by the Editor when edits and selection changes occur". Lacking
  // clear definition on which events fall through to here, we (for now) expect
  // to only handle selection changes here. Should seek for clarity re which
  // events can trigger this code path.
  _handleRawChange(editorState) {
    const selection = editorState.getSelection();
    const anchor = selection.getAnchorOffset();
    const focus = selection.getFocusOffset();
    this.props.onMoveCursor({anchor, focus});
  }

  render() {
    const {opaqueSyntax, templatedTreeInstance, langInstance} = this.props;
    const {
      block,
      selection: {
        anchorOffset, anchorKey,
        focusOffset, focusKey,
      },
      preEntityMap,
    } = genContentState(templatedTreeInstance)(opaqueSyntax);

    const entityMap = {};
    preEntityMap.value0.forEach((val, ix) => {
      entityMap[ix] = entityTypes[val];
    });
    const contentState = convertFromRaw({
      blocks: [Object.assign({}, block, {type: 'unstyled'})],
      entityMap,
    });
    let editorState = EditorState.createWithContent(
      contentState, DataDecorator
    );
    const selectionState = editorState.getSelection().merge({
      focusKey, focusOffset,
      anchorKey, anchorOffset,
      isBackward: focusOffset < anchorOffset,
    });

    // TODO we don't *always* want this selection to be force. sometimes you
    // want to click elsewhere. only if the editor has focus.
    editorState = EditorState.forceSelection(editorState, selectionState);

    const holeCount = listAllHoles(templatedTreeInstance)(opaqueSyntax).length;
    const conflictCount = listAllConflicts(opaqueSyntax).length;

    const holeCountStr = holeCount === 1
      ? `${holeCount} hole`
      : `${holeCount} holes`;

    const conflictCountStr = conflictCount === 1
      ? `${conflictCount} conflict`
      : `${conflictCount} conflicts`;

    /*
    const conflicts = listAllConflicts(this.props.opaqueSyntax)
      .map(({conflict, loc}) => {
        const {outsideTy, insideTy} = conflict.value0;
        return (
          <li>
            <ul style={styles.conflictList}>
              <li>
                <AdditionDisplay opaqueSyntax={conflict} />
              </li>
              <li style={styles.conflictItem}>
                <button onClick={() => this.handleResolve(loc, 'take-outside')}>take</button>
                <div>outside:</div>
                <div><AdditionDisplay opaqueSyntax={outsideTy} /></div>
              </li>
              <li style={styles.conflictItem}>
                <button onClick={() => this.handleResolve(loc, 'take-inside')}>take</button>
                <div>inside:</div>
                <div><AdditionDisplay opaqueSyntax={insideTy} /></div>
              </li>
            </ul>
          </li>
        );
      });
   */

    const {evaluated, selectionSuggestions} =
      selectionInfo(this.props.opaqueSyntax);

    const suggestion = selectionSuggestions === 'no-suggestion'
      ? null
      : <p>suggestion: {selectionSuggestions}</p>;

    return (
      <div style={styles.root}>
        <h3>editor</h3>
        <div style={styles.editor}>
          <Editor
            ref={elem => this.editor = elem}
            editorState={editorState}
            handleKeyCommand={this.handleKeyCommand}
            handleBeforeInput={this.handleBeforeInput}
            onTab={this.handleTab}
            keyBindingFn={specialKeyBindings}
            onChange={this.handleRawChange}
          />
        </div>
        <div style={styles.info}>
          <h3>info</h3>
          {suggestion}
          <div>evaluated selection: <AdditionDisplay opaqueSyntax={evaluated} /></div>
          <p>{holeCountStr}, {conflictCountStr}</p>
        </div>
      </div>
    );
  }
}

function AdditionDisplay({opaqueSyntax}) {
  const {
    block,
    preEntityMap,
  } = genDisplayContentState(intBoolIsTemplated)(opaqueSyntax);

  const entityMap = {};
  preEntityMap.value0.forEach((val, ix) => {
    entityMap[ix] = entityTypes[val];
  });
  const contentState = convertFromRaw({
    blocks: [Object.assign({}, block, {type: 'unstyled'})],
    entityMap,
  });
  let editorState = EditorState.createWithContent(
    contentState, DataDecorator
  );

  return <Editor editorState={editorState} />;
}

function handleEither(either, left, right) {
  return either.constructor.name === 'Left'
    ? left(either.value0)
    : right(either.value0);
}

export class StatefulStructuredEditor extends React.Component {
  constructor(props) {
    super();

    this.state = this._getState(props.selectSyntax);

    this.onChange = command => this._onChange(command);
    this.handleMoveCursor = anchorFocus => this._handleMoveCursor(anchorFocus);
  }

  // XXX this was messing up editing
  // componentWillReceiveProps({selectSyntax}) {
  //   this.setState(this._getState(selectSyntax));
  // }

  _getState(selectSyntax) {
    return handleEither(
      initSelectSyntax(selectSyntax),
      lastWarning => {
        console.log(lastWarning);
        return {
          // TODO we don't actually handle the case when opaqueSyntax is null
          opaqueSyntax: null,
          lastWarning,
        };
      },
      opaqueSyntax => {
        return {
          opaqueSyntax,
          lastWarning: null,
        };
      }
    );
  }

  _onChange(command) {
    handleEither(
      operate(intBoolIsOperate)(this.state.opaqueSyntax, command),
      lastWarning => this.setState({lastWarning}),
      opaqueSyntax => {
        this.setState({opaqueSyntax, lastWarning: null});
        this.props.onChange(opaqueSyntax, command);
      }
    );
  }

  _handleMoveCursor(anchorFocus) {
    handleEither(
      setEndpoints(this.props.templatedTreeInstance)(this.state.opaqueSyntax, anchorFocus),
      lastWarning => this.setState({lastWarning}),
      opaqueSyntax => {
        this.setState({
          opaqueSyntax,
          lastWarning: null,
        });
        this.props.onChange(
          opaqueSyntax,
          {tag: 'move-cursor', value: anchorFocus}
        );
      }
    );
  }

  componentDidMount() {
    this.editor.focus();
  }

  render() {
    const {templatedTreeInstance, langInstance} = this.props;
    const {opaqueSyntax, lastWarning} = this.state;
    return (
      <div style={styles.root}>
        <StructuredEditor
          onChange={this.onChange}
          onMoveCursor={this.handleMoveCursor}
          opaqueSyntax={opaqueSyntax}
          templatedTreeInstance={templatedTreeInstance}
          langInstance={langInstance}
          ref={ref => this.editor = ref}
        />
        {lastWarning && <pre style={styles.err}>{lastWarning}</pre>}
      </div>
    );
  }
}

const styles = {
  root: {
    fontFamily: 'monospace',
  },
  err: {
    borderLeft: '3px solid rgba(255, 112, 0, 0.43)',
  },
  number: {
    backgroundColor: 'rgba(0, 0, 255, 0.09)',
  },
  hole: {
    backgroundColor: 'rgba(255, 177, 0, 0.25)',
  },
  conflict: {
    backgroundColor: 'rgba(255, 0, 0, 0.09)',
  },
  info: {
  },
  editor: {
    paddingBottom: 10,
    borderBottom: '1px solid gray',
    overflow: 'hidden',
  },
  conflictItem: {
    display: 'flex',
  },
  conflictList: {
    listStyleType: 'none',
    WebkitPaddingStart: 0,
  },
};
