import { configure } from '@kadira/storybook';

function loadStories() {
  // require('./hole');
  // require('./editor');
  require('./structurededitor');
  require('./katexeditor');
}

configure(loadStories, module);
