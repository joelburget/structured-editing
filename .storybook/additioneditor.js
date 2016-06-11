import React from 'react';
import { storiesOf, action } from '@kadira/storybook';

import AdditionEditor from '../AdditionEditor';

const onChange = action('onChange');

storiesOf('AdditionEditor', module)
  .add('1 + 1', () => {
    const structure = {
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
      anchor: [0],
      focus: [0],
    };
    return <AdditionEditor onChange={onChange} structure={structure} />;
  }).add('1 + (2 + 3)', () => {
    const structure = {
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
      anchor: [0],
      focus: [2],
    };
    return <AdditionEditor onChange={onChange} structure={structure} />;
  });
