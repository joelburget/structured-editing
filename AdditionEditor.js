// @flow
import { List } from 'immutable';
import {
  convertFromRaw,
  // convertToRaw,
  CompositeDecorator,
  ContentState,
  Editor,
  EditorState,
  Entity,
  getDefaultKeyBinding,
  KeyBindingUtil,
} from 'draft-js';
import React from 'react';

import type {
  ContentBlock,
  RawDraftContentBlock,
} from 'draft-js';

import {operateJs} from './purescript/output/Main/index.js';

const {hasCommandModifier} = KeyBindingUtil;

// (1 + hole)
const initState = {
  tag: 'plus',
  l: {
    tag: 'number',
    value: 1,
  },
  r: {
    tag: 'hole',
    name: 'hole',
  },
};

console.log(operateJs(
  initState,
  {
    anchorOffset: 2,
    anchorKey: '',
    focusOffset: 2,
    focusKey: '',
  },
  {
    tag: 'typing',
    value: '0',
  }
));

console.log(operateJs(
  initState,
  {
    anchorOffset: 9,
    anchorKey: '',
    focusOffset: 9,
    focusKey: '',
  },
  {
    tag: 'typing',
    value: 'y',
  }
));

console.log(operateJs(
  initState,
  {
    anchorOffset: 9,
    anchorKey: '',
    focusOffset: 9,
    focusKey: '',
  },
  { tag: 'backspace', }
));

// (1 + _)
const emptyHole = {
  tag: 'plus',
  l: {
    tag: 'number',
    value: 1,
  },
  r: {
    tag: 'hole',
    name: '',
  },
};

console.log(operateJs(
  emptyHole,
  {
    anchorOffset: 5,
    anchorKey: '',
    focusOffset: 5,
    focusKey: '',
  },
  {
    tag: 'typing',
    value: '9',
  }
));

console.log(operateJs(
  emptyHole,
  {
    anchorOffset: 5,
    anchorKey: '',
    focusOffset: 5,
    focusKey: '',
  },
  { tag: 'backspace', }
));

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

type PathStep = 'left' | 'right' | number;
type Path = Array<PathStep>;

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

type Plus = {
  tag: 'plus';
  l: Syntax;
  r: Syntax;
};

type SNumber = {
  tag: 'number';
  value: number;
};

type Hole = {
  tag: 'hole';
  name: string;
};

type Syntax =  Plus | SNumber | Hole;

// A selection end relative to the current node is either
// * at some offset within it
// * away to the left
// * away to the right
// * or the focus is not within this textbox
// type SelectionEndOffset = number | 'left' | 'right' | 'unfocused';

type LightInline = {
  tag: string;
  text: string;

  // if these keys exist they denote the offsets within this inline string of
  // the anchor and / or focus.
  anchor?: number;
  focus?: number;
};
type LightBlock = Array<LightInline>;


export function blocksFromContent(
  lightBlocks: Array<LightBlock>
): Array<RawDraftContentBlock> {
  let currentOffset = 0;
  let anchorKey = null;
  let anchorOffset = 0;
  let focusKey = null;
  let focusOffset = 0;

  const blocks = lightBlocks.map(block => {
    let text = '';
    let entityRanges = [];

    // {text, tag}
    for (let inline of block) {
      if (inline.anchor != null) {
        anchorOffset = currentOffset + inline.anchor;
        anchorKey = inline.tag;
      }
      if (inline.focus != null) {
        focusOffset = currentOffset + inline.focus;
        focusKey = inline.tag;
      }
      currentOffset += text.length;
      entityRanges.push({
        offset: text.length,
        length: inline.text.length,
        key: inline.tag,
      });
      text += inline.text;
    }

    return {text, entityRanges, type: 'unstyled'};
  });

  return {
    blocks,
    anchorOffset, anchorKey,
    focusOffset, focusKey,
  };
}

type SyntaxAndSelection = {
  syntax: Syntax;
  anchor: Path;
  focus: Path;
};

function makeContent(encoding: SyntaxAndSelection): ContentState {
  const {syntax, anchor, focus} = encoding;
  const {
    blocks,
    anchorOffset, anchorKey,
    focusOffset, focusKey,
  } = blocksFromContent(contentFromSyntax(syntax, anchor, focus));
  return {
    content: convertFromRaw({blocks, entityMap}),
    anchorOffset, anchorKey,
    focusOffset, focusKey,
  };
}


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

export default class AdditionEditor extends React.Component {
  // state: {
  //   editorState: EditorState;
  // };
  // onChange: Function;

  constructor(props: {content: ContentState}) {
    super(props);

    this.onChange = command => this._onChange(command);
    this.handleKeyCommand = command => this._handleKeyCommand(command);
    this.handleBeforeInput = chars => this._handleBeforeInput(chars);
  }

  _onChange(command) {
    const {structure} = this.props;
    this.props.onChange(
      structure,
      command/*,
      operateJs(structure, XXX, command)*/
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
    const {structure} = this.props;
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
      content,
      anchorOffset, anchorKey,
      focusOffset, focusKey,
    } = makeContent(this.props.structure);
    let editorState = EditorState.createWithContent(
      content,
      DataDecorator
    );
    const selectionState = editorState.getSelection().merge({
      focusKey, focusOffset,
      anchorKey, anchorOffset,
      isBackward: focusOffset < anchorOffset,
    });
    editorState = EditorState.forceSelection(editorState, selectionState);
    console.log(editorState.toJS());

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
