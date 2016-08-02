import React from 'react';
import { storiesOf, action } from '@kadira/storybook';

import {StatefulAdditionEditor} from '../AdditionEditor';

const onChange = action('onChange');
// function onChange(syntax, evt) {

// }

// 1 + hole
const holeHole = {
  tag: 'internal',
  value: '',
  children: [
    {
      tag: 'leaf',
      value: 1,
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
  value: '',
  children: [
    {
      tag: 'leaf',
      value: 1,
    },
    {
      tag: 'hole',
      value: '_',
    },
  ]
};

storiesOf('AdditionEditor', module)
  .add('1 + hole|', () => {
    const selectSyntax = {
      syntax: holeHole,
      anchor: 8,
      focus: 8,
    };
    return (
      <StatefulAdditionEditor
        onChange={onChange}
        selectSyntax={selectSyntax}
      />
    );
  })
  .add('1 + _|', () => {
    const selectSyntax = {
      syntax: emptyHole,
      anchor: 5,
      focus: 5,
    };
    return (
      <StatefulAdditionEditor
        onChange={onChange}
        selectSyntax={selectSyntax}
      />
    );
  })
  .add('|1 + 1', () => {
    const selectSyntax = {
      syntax: {
        tag: 'internal',
        value: '',
        children: [
          {
            tag: 'leaf',
            value: 1,
          },
          {
            tag: 'leaf',
            value: 1,
          },
        ]
      },

      // point to the start of the block
      anchor: 0,
      focus: 0,
    };
    return (
      <StatefulAdditionEditor
        onChange={onChange}
        selectSyntax={selectSyntax}
      />
    );
  }).add('|1| + 2 + 3', () => {
    const selectSyntax = {
      syntax: {
        tag: 'internal',
        value: '',
        children: [
          {
            tag: 'leaf',
            value: 1,
          },
          {
            tag: 'internal',
            value: '',
            children: [
              {
                tag: 'leaf',
                value: 2,
              },
              {
                tag: 'leaf',
                value: 3,
              },
            ]
          },
        ]
      },

      // point to the start of the block
      anchor: 0,
      focus: 1,
    };
    return (
      <StatefulAdditionEditor
        onChange={onChange}
        selectSyntax={selectSyntax}
      />
    );
  });
