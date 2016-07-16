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

import {initJs, operateJs, contentStateFromSelectSyntaxJs} from './purescript/output/Main/index.js';
// contentStateFromSelectSyntax :: SelectSyntax -> Either String ContentState
// init :: Syntax -> RawSelection -> Either String ContentState
// rawOperate :: Syntax -> RawSelection -> Action -> Either String (Tuple Syntax ContentState)

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

    const contentState = initJs(
      props.selectSyntax.syntax,
      Object.assign({}, defaultSelection, props.selectSyntax.selection)
    );
    this.state = {contentState};

    this.onChange = command => this._onChange(command);
    this.handleKeyCommand = command => this._handleKeyCommand(command);
    this.handleBeforeInput = chars => this._handleBeforeInput(chars);
  }

  _onChange(command) {
    const {syntax} = this.props.selectSyntax;
    const {
      value0: newSyntax,
      value1: contentState,
    } = operateJs(syntax, this.state.contentState.selection, command);
    // XXX gross!
    this.setState({contentState});
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

  _handleRawOnChange() {
    console.log('raw onChange', arguments);
  }

  render() {
    const {
      block,
      selection: {
        anchorOffset, anchorKey,
        focusOffset, focusKey,
      },
    } = this.state.contentState;
    const contentState = convertFromRaw({
      blocks: [Object.assign({}, block, {type: 'unstyled'})],
      entityMap: {},
    });
    let editorState = EditorState.createWithContent(
      contentState
      /*, DataDecorator*/
    );
    // const selectionState = editorState.getSelection().merge({
    //   focusKey, focusOffset,
    //   anchorKey, anchorOffset,
    //   isBackward: focusOffset < anchorOffset,
    // });
    // editorState = EditorState.forceSelection(editorState, selectionState);
    console.log(editorState.toJS(), editorState.getCurrentContent().getFirstBlock().getText());

    return (
      <div style={styles.root}>
        <Editor
          editorState={editorState}
          handleKeyCommand={this.handleKeyCommand}
          handleBeforeInput={this.handleBeforeInput}
          keyBindingFn={additionKeyBindingFn}
          onChange={this._handleRawOnChange}
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
