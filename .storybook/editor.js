import React from 'react';
import { storiesOf, action } from '@kadira/storybook';
import {
  convertFromRaw,
  convertToRaw,
  ContentState,
} from 'draft-js';

import Editor from '../Editor';

const onChange = action('onChange');

function contentFromPrimitive({ tag, contents }) {
  switch (tag) {
    case 'Nat':
      const text = '' + contents;
      return [{
        text,
        entityRanges: [{
          offset: 0,
          length: text.length,
          key: tag,
        }],
      }];

    case 'String':
      return [{
        text: contents,
        entityRanges: [{
          offset: 0,
          length: contents.length,
          key: tag,
        }],
      }];

    default:
      throw Error('unhandled primitive tag');
  }
}

function contentFromValue({ tag, contents }) {
  switch (tag) {
    case 'Primitive':
      return contentFromPrimitive(contents);

    case 'Tuple':
      const [linearity, tuplePieces] = contents;
      let ret = [{
        text: '(',
        entityRanges: [{
          offset: 0,
          length: 1,
          key: tag,
        }]
      }];
      tuplePieces.forEach(piece => { ret = ret.concat(contentFromValue(piece)); });
      ret = ret.concat([{
          text: ')',
          entityRanges: [{
            offset: 0,
            length: 1,
            key: tag,
          }]
      }]);
      return ret;

    case 'Lam':
    case 'Primop':
    case 'Let':
    case 'Index':
    case 'Neu':
    case 'Plus':
    default:
      throw Error('unhandled value tag');
  }
}

function contentFromComputation({ tag, contents }) {
  switch (tag) {
    case 'FVar':
      return [{
        text: contents,
        entityRanges: [{
          offset: 0,
          length: contents.length,
          key: tag,
        }],
      }];

    case 'BVar':
    case 'App':
    case 'Annot':
    default:
      throw Error('unhandled computation tag');
  }
}

function convertFromBlocks(blocks) {
  let text = "";
  let entityRanges = [];

  blocks.forEach(block => {
    entityRanges = entityRanges.concat(block.entityRanges);
    // Tricky
    entityRanges[entityRanges.length - 1].offset = text.length;
    text = text + block.text;
  });

  return convertFromRaw({
    blocks: [{
      text,
      entityRanges,
      type: 'unstyled',
    }],
    entityMap: {
      FVar: {
        type: 'TOKEN',
        mutability: 'MUTABLE',
        data: {type: 'FVar'},
      },
      Primitive: {
        type: 'TOKEN',
        mutability: 'MUTABLE',
        data: {type: 'Primitive'},
      },
      Tuple: {
        type: 'TOKEN',
        mutability: 'MUTABLE',
        data: {type: 'Tuple'},
      },
    },
  });
}

storiesOf('Editor', module)
  .add('empty', () => {
    const rawContent = {
      blocks: [
        {
          text: '',
          type: 'unstyled'
        }
      ],
      entityMap: {},
    };
    const content = convertFromRaw(rawContent);
    return <Editor onChange={onChange} content={content} />;
  }).add('(FVar "x")', () => {
    const content = convertFromBlocks(contentFromComputation({
      tag: 'FVar',
      contents: 'x',
    }));
    return <Editor onChange={onChange} content={content} />;
  }).add('(Primitive (Nat 5))', () => {
    const content = convertFromBlocks(contentFromValue({
      tag: 'Primitive',
      contents: {
        tag: 'Nat',
        contents: 5,
      },
    }));
    return <Editor onChange={onChange} content={content} />;
  }).add('(Primitive (String "hazel"))', () => {
    const content = convertFromBlocks(contentFromValue({
      tag: 'Primitive',
      contents: {
        tag: 'String',
        contents: "hazel",
      },
    }));
    return <Editor onChange={onChange} content={content} />;
  }).add('(Tuple Nonlinear (V.fromList [NatV 5, NatV 5]))', () => {
    const content = convertFromBlocks(contentFromValue({
      "tag":"Tuple",
      "contents": [
        "Nonlinear",
        [
          {"tag":"Primitive","contents":{"tag":"Nat","contents":5}},
          {"tag":"Primitive","contents":{"tag":"Nat","contents":6}}
        ]
      ]
    }));
    return <Editor onChange={onChange} content={content} />;
  });
