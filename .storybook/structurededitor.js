import React from 'react';
import {storiesOf, action} from '@kadira/storybook';
import { Editor, Raw } from 'slate';
import LanguagePlugin from '../LanguagePlugin';

const onChange = action('onChange');

import {
  onePlusTwo,
} from '../purescript/output/Syntax';
import {mkSyntaxEditorState} from '../StructuredEditor';


class TestEditor extends React.Component {
  constructor(props) {
    super(props);
    const opaqueSyntax = initSyntax(props.syntax).value0;
    const initialState = mkSyntaxEditorState({
      templatedInstance: intBoolIsTemplated,
      opaqueSyntax,
    });
    this.state = {state: initialState};
    this.onChange = state => this._onChange(state);
  }

  _onChange(state) {
    onChange(state);
    this.setState({state});
  }

  render() {
    return (
      <div style={{fontFamily: 'monospace'}}>
        <Editor
          state={this.state.state}
          plugins={[LanguagePlugin()]}
          onChange={this.onChange}
        />
      </div>
    );
  }
}


// 1 + hole
const holeHole = {
  tag: 'internal',
  value: {tag: 'addition'},
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
  value: {tag: 'addition'},
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
  .add('1 + hole', () => {
    return <TestEditor syntax={holeHole} />;
  })
  .add('1 + _', () => {
    return <TestEditor syntax={emptyHole} />;
  })
  .add('if true then hole 1 else 2', () => {
    const syntax = {
      tag: 'internal',
      value: {tag: 'ifthenelse'},
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
    };
    return <TestEditor syntax={syntax} />;
  })
  .add('false == _', () => {
    const syntax = {
      tag: 'internal',
      value: {tag: 'eq'},
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
    };
    return <TestEditor syntax={syntax} />;
  })
  .add('1 + 1', () => {
    const syntax = {
      tag: 'internal',
      value: {tag: 'addition'},
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
    };
    return <TestEditor syntax={syntax} />;
  }).add('1 + 2 + 3', () => {
    const syntax = {
      tag: 'internal',
      value: {tag: 'addition'},
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
          value: {tag: 'addition'},
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
    };
    return <TestEditor syntax={syntax} />;
  });
