import { configure } from '@kadira/storybook';

function loadStories() {
  require('./hole');
  require('./editor');
  require('./additioneditor');
}

configure(loadStories, module);
