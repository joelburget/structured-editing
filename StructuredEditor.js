// @flow
import React from 'react';
import { List } from 'immutable';
import {
  convertFromRaw,
  CompositeDecorator,
  ContentBlock,
  ContentState,
  Editor,
  EditorState,
  Entity,
  getDefaultKeyBinding,
  KeyBindingUtil,
} from 'draft-js';

import {
  listAllHoles,
  listAllConflicts,
  genContentState,
  genDisplayContentState,
  setEndpoints,
  operate,
} from './purescript/output/Interface';

const {hasCommandModifier} = KeyBindingUtil;

const entityTypes = {
  leaf: {
    type: 'TOKEN',
    mutability: 'MUTABLE',
    data: {type: 'leaf'},
  },
  hole: {
    type: 'TOKEN',
    mutability: 'MUTABLE',
    data: {type: 'hole'},
  },
  internal: {
    type: 'TOKEN',
    mutability: 'MUTABLE',
    data: {type: 'internal'},
  },
  conflict: {
    type: 'TOKEN',
    mutability: 'MUTABLE',
    data: {type: 'conflict'},
  },
};

export function mkDataDecorator(components) {
  return {
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
}


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
    const {opaqueZipper, templatedInstance, langInstance} = this.props;
    // TODO we don't *always* want this selection to be force. sometimes you
    // want to click elsewhere. only if the editor has focus.
    const editorState = mkZipperEditorState(this.props);

    const holeCount = listAllHoles(templatedInstance)(opaqueZipper).length;
    const conflictCount = listAllConflicts(opaqueZipper).length;

    const holeCountStr = holeCount === 1
      ? `${holeCount} hole`
      : `${holeCount} holes`;

    const conflictCountStr = conflictCount === 1
      ? `${conflictCount} conflict`
      : `${conflictCount} conflicts`;

    // const {evaluated, selectionSuggestions} =
    //   selectionInfo(this.props.opaqueZipper);

    // const suggestion = selectionSuggestions === 'no-suggestion'
    //   ? null
    //   : <p>suggestion: {selectionSuggestions}</p>;

    // {suggestion}
    // <div>evaluated selection: <AdditionDisplay opaqueSyntax={evaluated} /></div>

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
          <p>{holeCountStr}, {conflictCountStr}</p>
        </div>
      </div>
    );
  }
}


function mkContentState({preEntityMap, block}) {
  const entityMap = {};
  preEntityMap.value0.forEach((val, ix) => {
    entityMap[ix] = entityTypes[val];
  });
  return convertFromRaw({
    blocks: [Object.assign({}, block, {type: 'unstyled'})],
    entityMap,
  });
}

function mkSyntaxEditorState({templatedInstance, opaqueSyntax, DataDecorator}) {
  const preContentState =
    genDisplayContentState(templatedInstance)(opaqueSyntax);

  return EditorState.createWithContent(
    mkContentState(preContentState),
    DataDecorator
  );
}

function mkZipperEditorState({templatedInstance, opaqueZipper, DataDecorator}) {
  const {
    selection,
    ...preContentState,
  } = genContentState(templatedInstance)(opaqueZipper);

  const editorState = EditorState.createWithContent(
    mkContentState(preContentState),
    DataDecorator
  );

  const {anchorOffset, focusOffset} = selection;
  const selectionState = editorState.getSelection().merge({
    ...selection,
    isBackward: focusOffset < anchorOffset,
  });
  return EditorState.forceSelection(editorState, selectionState);
}

export function StructureDisplay(props) {
  return (
    <Editor
      editorState={mkSyntaxEditorState(props)}
      // TODO should we handle events?
      onChange={() => {}}
    />
  );
}

function handleEither(either, left, right) {
  return either.constructor.name === 'Left'
    ? left(either.value0)
    : right(either.value0);
}

export class StatefulStructuredEditor extends React.Component {
  constructor(props) {
    super(props);

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
      this.props.initSelectSyntax(selectSyntax),
      lastWarning => {
        console.log(lastWarning);
        return {
          // TODO we don't actually handle the case when opaqueZipper is null
          opaqueZipper: null,
          lastWarning,
        };
      },
      opaqueZipper => {
        return {
          opaqueZipper,
          lastWarning: null,
        };
      }
    );
  }

  _onChange(command) {
    handleEither(
      operate(this.props.operateInstance)(this.state.opaqueZipper, command),
      lastWarning => this.setState({lastWarning}),
      opaqueZipper => {
        this.setState({opaqueZipper, lastWarning: null});
        this.props.onChange(opaqueZipper, command);
      }
    );
  }

  _handleMoveCursor(anchorFocus) {
    handleEither(
      setEndpoints(this.props.templatedInstance)(this.state.opaqueZipper, anchorFocus),
      lastWarning => this.setState({lastWarning}),
      opaqueZipper => {
        this.setState({
          opaqueZipper,
          lastWarning: null,
        });
        this.props.onChange(
          opaqueZipper,
          {tag: 'move-cursor', value: anchorFocus}
        );
      }
    );
  }

  componentDidMount() {
    this.editor.focus();
  }

  render() {
    const {templatedInstance, langInstance, operateInstance, DataDecorator} = this.props;
    const {opaqueZipper, lastWarning} = this.state;
    return (
      <div style={styles.root}>
        <StructuredEditor
          onChange={this.onChange}
          onMoveCursor={this.handleMoveCursor}
          DataDecorator={DataDecorator}
          opaqueZipper={opaqueZipper}
          templatedInstance={templatedInstance}
          langInstance={langInstance}
          operateInstance={operateInstance}
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
