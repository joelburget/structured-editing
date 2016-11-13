import React from 'react';
import { storiesOf, action } from '@kadira/storybook';

import Hole from '../Hole';

storiesOf('Hole', module)
  .add('empty hole', () => (
    <Hole onSelect={action('select')} />
  ));
