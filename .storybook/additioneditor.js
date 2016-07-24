import React from 'react';
import { storiesOf, action } from '@kadira/storybook';

import {StatefulAdditionEditor} from '../AdditionEditor';

const onChange = action('onChange');
// function onChange(syntax, evt) {

// }

// (1 + hole)
const holeHole = {
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

// (1 + _)
const emptyHole = {
  tag: 'plus',
  l: {
    tag: 'number',
    value: 1,
  },
  r: {
    tag: 'hole',
    name: '_',
  },
};

storiesOf('AdditionEditor', module)
  .add('(1 + hole|)', () => {
    const selectSyntax = {
      syntax: holeHole,
      anchor: 9,
      focus: 9,
    };
    return (
      <StatefulAdditionEditor
        onChange={onChange}
        selectSyntax={selectSyntax}
      />
    );
  })
  .add('(1 + _|)', () => {
    const selectSyntax = {
      syntax: emptyHole,
      anchor: 6,
      focus: 6,
    };
    return (
      <StatefulAdditionEditor
        onChange={onChange}
        selectSyntax={selectSyntax}
      />
    );
  })
  .add('|(1 + 1)', () => {
    const selectSyntax = {
      syntax: {
        tag: 'plus',
        l: {
          tag: 'number',
          value: 1,
        },
        r: {
          tag: 'number',
          value: 1,
        },
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
  }).add('|(1| + (2 + 3))', () => {
    const selectSyntax = {
      syntax: {
        tag: 'plus',
        l: {
          tag: 'number',
          value: 1,
        },
        r: {
          tag: 'plus',
          l: {
            tag: 'number',
            value: 2,
          },
          r: {
            tag: 'number',
            value: 3,
          },
        },
      },

      // point to the start of the block
      anchor: 0,
      focus: 2,
    };
    return (
      <StatefulAdditionEditor
        onChange={onChange}
        selectSyntax={selectSyntax}
      />
    );
  });
