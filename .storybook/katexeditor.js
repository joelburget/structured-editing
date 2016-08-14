import React from 'react';
import { storiesOf, action } from '@kadira/storybook';

import {
  StatefulStructuredEditor,
  StructureDisplay,
} from '../StructuredEditor';
import {
  initSelectSyntax,
  katexIsTemplated,
  toOpaque,
} from '../purescript/output/Katex';
import {KatexDisplay} from '../KatexEditor';

const onChange = action('onChange');

// function TestEditor({selectSyntax}) {
//   return (
//     <StatefulStructuredEditor
//       onChange={onChange}
//       selectSyntax={selectSyntax}
//       templatedTreeInstance={intBoolIsTemplated}
//       langInstance={intBoolIsLang}
//     />
//   );
// }

function TestDisplay({syntax}) {
  return <KatexDisplay opaqueSyntax={toOpaque(syntax).value0} />;
}


// \sqrt{2}
const sqrt2 = {
  tag: 'internal',
  value: 'sqrt',
  children: [
    {
      tag: 'leaf',
      value: {
        tag: 'str',
        value: '2',
      },
    },
  ]
};

storiesOf('Katex Editor', module)
  .add('sqrt', () => {
    return <TestDisplay syntax={sqrt2} />;
  });
