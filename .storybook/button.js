import React from 'react';
import { storiesOf, action } from '@kadira/storybook';

import Computation from '../Computation';

storiesOf('Computation', module)
  .add('with text', () => (
    <Computation action={action} />
  ))
  .add('with no text', () => (
    <button></button>
  ));
