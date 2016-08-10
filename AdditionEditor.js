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
  selectionInfo,
  listAllHoles,
  listAllConflicts,
  operate,
  genContentState,
  genDisplayContentState,
  initSelectSyntax,
  setEndpoints,
} from './purescript/output/Main/index.js';

const {hasCommandModifier} = KeyBindingUtil;

// Practice / learn by writing an editor for binary addition expressions with
// holes
//
// Examples:
// * 1 + 1
// * (1 + _) + 1
// * (1 + _) + (3 + 4)
//
// Conflicts are going to be very important! They're where propagation stops.
// Are they different from holes?

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


// Operations to support:
// * backspace
// * plus
// * number
// * open paren
function additionKeyBindingFn(e: SyntheticKeyboardEvent): string {
  if (!hasCommandModifier(e)) {
    /*if (e.keyCode >= 0x30 && e.keyCode <= 0x39) {
      return 'additioneditor-number';
    } else if (e.key === '(') {
      return 'additioneditor-openparen';
    } else if (e.key === '+') {
      return 'additioneditor-plus';
    } else*/ if (e.key === 'Backspace') {
      return 'additioneditor-backspace';
    }
  }

  return getDefaultKeyBinding(e);
}

const defaultSelection = {
  anchorKey: '',
  anchorOffset: 0,
  focusKey: '',
  focusOffset: 0,
};

export class AdditionEditor extends React.Component {
  constructor(props) {
    super(props);

    this.handleKeyCommand = command => this._handleKeyCommand(command);
    this.handleBeforeInput = chars => this._handleBeforeInput(chars);
    this.handleRawChange = editorState => this._handleRawChange(editorState);
    this.focus = () => this.editor.focus();
  }

  _handleBeforeInput(chars: string): boolean {
    this.props.onChange({
      tag: 'typing',
      value: chars,
    });
    return true;
  }

  _handleKeyCommand(command: string): boolean {
    // const {syntax} = this.props;
    switch (command) {
      // case 'additioneditor-number':
      // case 'additioneditor-openparen':
      // case 'additioneditor-plus':
      case 'additioneditor-backspace':
        this.props.onChange({tag: 'backspace'});
        return true;

      default:
        return false;
    }
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
    const {
      block,
      selection: {
        anchorOffset, anchorKey,
        focusOffset, focusKey,
      },
      preEntityMap,
    } = genContentState(this.props.opaqueSyntax);

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

    const holes = listAllHoles(this.props.opaqueSyntax)
      .map(hole => <li><AdditionDisplay opaqueSyntax={hole} /></li>);

    const conflicts = listAllConflicts(this.props.opaqueSyntax)
      .map(({conflictInfo, loc}) => (
        <li>
          <ul style={styles.conflictList}>
            <li style={styles.conflictItem}>
              <div>expected:</div>
              <div><AdditionDisplay opaqueSyntax={conflictInfo.expectedTy} /></div>
            </li>
            <li style={styles.conflictItem}>
              <div>actual:</div>
              <div><AdditionDisplay opaqueSyntax={conflictInfo.actualTy} /></div>
            </li>
          </ul>
        </li>
      ));

    const {anchorInfo, focusInfo, evaluated} =
      selectionInfo(this.props.opaqueSyntax);

    return (
      <div style={styles.root}>
        <h3>editor</h3>
        <div style={styles.editor}>
          <Editor
            ref={elem => this.editor = elem}
            editorState={editorState}
            handleKeyCommand={this.handleKeyCommand}
            handleBeforeInput={this.handleBeforeInput}
            keyBindingFn={additionKeyBindingFn}
            onChange={this.handleRawChange}
          />
        </div>
        <div style={styles.info}>
          <h3>info</h3>
          <div>anchor: <AdditionDisplay opaqueSyntax={anchorInfo} /></div>
          <div>focus: <AdditionDisplay opaqueSyntax={focusInfo} /></div>
          <div>evaluated: <AdditionDisplay opaqueSyntax={evaluated} /></div>
          <h3>conflicts</h3>
          <ul style={styles.conflictList}>
            {conflicts}
          </ul>
          <h3>holes</h3>
          <ul style={styles.conflictList}>
            {holes}
          </ul>
        </div>
      </div>
    );
  }
}

function AdditionDisplay({opaqueSyntax}) {
  const {
    block,
    preEntityMap,
  } = genDisplayContentState(opaqueSyntax);

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

export class StatefulAdditionEditor extends React.Component {
  constructor({onChange, selectSyntax}) {
    super();

    this.state = this._getState(selectSyntax);

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
      operate(this.state.opaqueSyntax, command),
      lastWarning => this.setState({lastWarning}),
      opaqueSyntax => {
        this.setState({opaqueSyntax, lastWarning: null});
        this.props.onChange(opaqueSyntax, command);
      }
    );
  }

  _handleMoveCursor(anchorFocus) {
    handleEither(
      setEndpoints(this.state.opaqueSyntax, anchorFocus),
      lastWarning => this.setState({lastWarning}),
      opaqueSyntax => {
        this.setState({
          opaqueSyntax,
          lastWarning: null,
        });
        // this.props.onChange(opaqueSyntax, TODO);
      }
    );
  }

  componentDidMount() {
    this.editor.focus();
  }

  render() {
    const {opaqueSyntax, lastWarning} = this.state;
    return (
      <div style={styles.root}>
        <AdditionEditor
          onChange={this.onChange}
          onMoveCursor={this.handleMoveCursor}
          opaqueSyntax={opaqueSyntax}
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
