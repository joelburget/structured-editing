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

import {operateJs, contentStateFromSelectSyntaxJs} from './purescript/output/Main/index.js';

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

const entityMap = {
  number: {
    type: 'TOKEN',
    mutability: 'MUTABLE',
    data: {type: 'number'},
  },
  plus: {
    type: 'TOKEN',
    mutability: 'MUTABLE',
    data: {type: 'plus'},
  },
};

const components = {
  plus: ({children}) => {
    // assert React.Children.count(children) === 2
    // const [l, r] = children;
    return <span>{children}</span>;
  },
  number: ({children}) => {
    return <span>{children}</span>
  },
  hole: ({children}) => (
    <span>({React.Children.only(children)})</span>
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

export default class AdditionEditor extends React.Component {
  constructor(props) {
    super(props);

    this.state = props.selectSyntax;
    // Object.assign({}, defaultSelection, props.selectSyntax.selection),

    this.onChange = command => this._onChange(command);
    this.handleKeyCommand = command => this._handleKeyCommand(command);
    this.handleBeforeInput = chars => this._handleBeforeInput(chars);
    this.handleRawChange = editorState => this._handleRawChange(editorState);
    this.focus = () => this.editor.focus();
  }

  _onChange(command) {
    const {syntax, anchor, focus} = this.props.selectSyntax;
    const selection = {anchor, focus};
    const {
      value0: newSyntax,
      value1: newSelection,
    } = operateJs(this.props.selectSyntax, command);
    // TODO don't set state and call back props
    if (newSelection == null || newSelection == null) {
      debugger;
    }
    this.setState({
      syntax: newSyntax,
      selection: newSelection,
    });
    this.props.onChange(
      newSyntax,
      command
    );
  }

  _handleBeforeInput(chars: string): boolean {
    this.onChange({
      tag: 'typing',
      value: chars,
    });
    return true;
  }

  _handleKeyCommand(command: string): boolean {
    const {syntax} = this.props;
    switch (command) {
      // case 'additioneditor-number':
      // case 'additioneditor-openparen':
      // case 'additioneditor-plus':
      case 'additioneditor-backspace':
        this.onChange({tag: 'backspace'});
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
    const selection = editorState.getSelection()
    const anchor = selection.getAnchorOffset();
    const focus = selection.getFocusOffset();
    const {
      anchor: oldAnchor,
      focus: oldFocus,
    } = this.state;
    console.log(anchor , oldAnchor , focus , oldFocus);
    if (anchor !== oldAnchor || focus !== oldFocus) {
      this.setState({anchor, focus});
    } else {
      console.log('unhandled raw onChange', editorState.toJS());
    }
  }

// contentStateFromSelectSyntax :: SelectSyntax -> Either String ContentState
// init :: Syntax -> RawSelection -> Either String ContentState
// operate :: Syntax -> RawSelection -> Action -> Either String (Tuple Syntax ContentState)

  render() {
    const x =  contentStateFromSelectSyntaxJs(this.state);
    const {
      block,
      selection: {
        anchorOffset, anchorKey,
        focusOffset, focusKey,
      },
    } = x;
    const contentState = convertFromRaw({
      blocks: [Object.assign({}, block, {type: 'unstyled'})],
      entityMap: {},
    });
    let editorState = EditorState.createWithContent(
      contentState
      /*, DataDecorator*/
    );
    const selectionState = editorState.getSelection().merge({
      focusKey, focusOffset,
      anchorKey, anchorOffset,
      isBackward: focusOffset < anchorOffset,
    });
    editorState = EditorState.forceSelection(editorState, selectionState);

    return (
      <div style={styles.root}>
        <Editor
          ref={elem => this.editor = elem}
          editorState={editorState}
          handleKeyCommand={this.handleKeyCommand}
          handleBeforeInput={this.handleBeforeInput}
          keyBindingFn={additionKeyBindingFn}
          onChange={this.handleRawChange}
        />
      </div>
    );
  }
}

const styles = {
  root: {
    fontFamily: 'monospace',
  },
};
