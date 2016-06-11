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

console.log(operateJs(
      {
        tag: 'plus',
        l: {
          tag: 'number',
          value: 1,
        },
        r: {
          tag: 'hole',
          name: 'hole',
        },
      },
      // TODO change to l/r ?
      ['left', 1],
      {
        tag: 'typing',
        value: '0',
      }
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

function flattenOneLevel<A>(arrs: Array<Array<A>>): Array<A> {
  return Array.prototype.concat.apply([], arrs);
}

function subPath(step: string, path: ?Path): ?Path {
  // we've reached the end of the path
  if (path == null || path.length === 1) {
    return null;
  }

  // the first item in the path matched -- drop it and continue
  if (path[0] === step) {
    return path.splice(1);
  }

  // otherwise not a match
  return null;
}

function getOffset(path: ?path): ?number {
  if (path == null || path.length > 1) {
    return null;
  }

  return path[0];
}

function inlineSelectionObj(start, len, anchorOffset, focusOffset) {
  const obj = {};
  if (anchorOffset != null &&
      start <= anchorOffset &&
      anchorOffset <= start + len) {
    obj.anchor = anchorOffset - start;
  }
  if (focusOffset != null &&
      start <= focusOffset &&
      focusOffset <= start + len) {
    obj.focus = focusOffset - start;
  }
  return obj;
}

// TODO this would be better expressed in Purescript
// TODO highlight things if they're in the selection
export function contentFromSyntax(
  syntax: Syntax,
  anchor: ?Path,
  focus: ?Path
): Array<LightBlock> {
  const { tag } = syntax;
  const anchorOffset = getOffset(anchor);
  const focusOffset = getOffset(focus);

  switch (syntax.tag) {
    case 'plus': {
      const l = contentFromSyntax(
        syntax.l,
        subPath('left', anchor),
        subPath('left', focus)
      )[0];
      const r = contentFromSyntax(
        syntax.r,
        subPath('right', anchor),
        subPath('right', focus)
      )[0];

      // XXX figure out what to do about offsets in here
      //
      // each recursive call returns Array<LightBlock>,
      // ie Array<Array<LightInline>>. We need to flatten that. Always? Assume
      // we get one block back for now... (TODO)
      const block = flattenOneLevel([
        [{
          text: '(',
          tag,
          ...inlineSelectionObj(0, 1, anchorOffset, focusOffset),
        }],
        l,
        [{
          text: ' + ',
          tag,
          ...inlineSelectionObj(1, 3, anchorOffset, focusOffset),
        }],
        r,
        [{
          text: ')',
          tag,
          ...inlineSelectionObj(4, 1, anchorOffset, focusOffset),
        }],
      ]);
      return [block];
    }

    case 'number': {
      const text = '' + syntax.value;
      return [[{
        text,
        tag,
        ...inlineSelectionObj(0, text.length, anchorOffset, focusOffset),
      }]];
    }

    case 'hole': {
      const text = syntax.name;
      return [[{
        text,
        tag,
        ...inlineSelectionObj(0, text.length, anchorOffset, focusOffset),
      }]];
    }

    default:
      throw Error('unhandled primitive tag');
  }
}


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
