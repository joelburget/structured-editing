import { configure } from '@kadira/storybook';

function loadStories() {
  require('./hole');
  require('./editor');
  require('./structurededitor');
}

configure(loadStories, module);
