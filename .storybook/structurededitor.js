import React from 'react';
import {storiesOf, action} from '@kadira/storybook';

import {StatefulIntBoolEditor} from '../IntBoolEditor';

const onChange = action('onChange');

function TestEditor({selectSyntax}) {
  return (
    <StatefulIntBoolEditor
      onChange={onChange}
      selectSyntax={selectSyntax}
    />
  );
}


// 1 + hole
const holeHole = {
  tag: 'internal',
  value: 'addition',
  children: [
    {
      tag: 'leaf',
      value: {
        tag: 'int',
        value: 1,
      },
    },
    {
      tag: 'hole',
      value: 'hole',
    },
  ]
};

// 1 + _
const emptyHole = {
  tag: 'internal',
  value: 'addition',
  children: [
    {
      tag: 'leaf',
      value: {
        tag: 'int',
        value: 1,
      },
    },
    {
      tag: 'hole',
      value: '_',
    },
  ]
};

storiesOf('Int / Bool Editor', module)
  .add('1 + hole|', () => {
    const selectSyntax = {
      syntax: holeHole,
      anchor: 8,
      focus: 8,
    };
    return <TestEditor selectSyntax={selectSyntax} />;
  })
  .add('1 + _|', () => {
    const selectSyntax = {
      syntax: emptyHole,
      anchor: 5,
      focus: 5,
    };
    return <TestEditor selectSyntax={selectSyntax} />;
  })
  .add('|if true then hole 1 else 2', () => {
    const selectSyntax = {
      syntax: {
        tag: 'internal',
        value: 'ifthenelse',
        children: [
          {
            tag: 'leaf',
            value: {
              tag: 'bool',
              value: true,
            },
          },
          {
            tag: 'hole',
            value: 'hole 1',
          },
          {
            tag: 'leaf',
            value: {
              tag: 'int',
              value: 2,
            },
          },
        ]
      },

      // point to the start of the block
      anchor: 0,
      focus: 0,
    };
    return <TestEditor selectSyntax={selectSyntax} />;
  })
  .add('false == _|', () => {
    const selectSyntax = {
      syntax: {
        tag: 'internal',
        value: 'eq',
        children: [
          {
            tag: 'leaf',
            value: {
              tag: 'bool',
              value: false,
            },
          },
          {
            tag: 'hole',
            value: "_",
          },
        ]
      },

      // point to the start of the block
      anchor: 10,
      focus: 10,
    };
    return <TestEditor selectSyntax={selectSyntax} />;
  })
  .add('|1 + 1', () => {
    const selectSyntax = {
      syntax: {
        tag: 'internal',
        value: 'addition',
        children: [
          {
            tag: 'leaf',
            value: {
              tag: 'int',
              value: 1,
            },
          },
          {
            tag: 'leaf',
            value: {
              tag: 'int',
              value: 1,
            },
          },
        ]
      },

      // point to the start of the block
      anchor: 0,
      focus: 0,
    };
    return <TestEditor selectSyntax={selectSyntax} />;
  }).add('|1| + 2 + 3', () => {
    const selectSyntax = {
      syntax: {
        tag: 'internal',
        value: 'addition',
        children: [
          {
            tag: 'leaf',
            value: {
              tag: 'int',
              value: 1,
            },
          },
          {
            tag: 'internal',
            value: 'addition',
            children: [
              {
                tag: 'leaf',
                value: {
                  tag: 'int',
                  value: 2,
                },
              },
              {
                tag: 'leaf',
                value: {
                  tag: 'int',
                  value: 3,
                },
              },
            ]
          },
        ]
      },

      // point to the start of the block
      anchor: 0,
      focus: 1,
    };
    return <TestEditor selectSyntax={selectSyntax} />;
  });
