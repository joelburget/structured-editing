import { List } from 'immutable';
import {
  CompositeDecorator,
  ContentState,
  Editor,
  EditorState,
  Entity,
} from 'draft-js';
import React from 'react';

const components = {
  FVar: ({children}) => (
    <span style={styles.fvar}>{children}</span>
  ),
  Primitive: ({children}) => {
    return <span style={styles.primitive}>{children}</span>
  },
};

const DataDecorator = {
  getDecorations(block: ContentBlock): List<?string> {
    const length = block.getLength();
    const arr = [];
    for (let i = 0; i < length; i ++) {
      const key = block.getEntityAt(i);
      arr.push(key);
    }

    return List(arr);
  },

  getComponentForKey(key: string): Function {
    const entity = Entity.get(key);
    const tyName = entity.data.type;
    return components[tyName];
  },

  getPropsForKey(key: string): ?Object {
    return null;
  },
};

// editor to fill a hole where a term is expected
export default class HazelEditor extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      editorState: EditorState.createWithContent(
        props.content,
        DataDecorator
      )
    };
    this.onChange = change => this._onChange(change);
  }

  _onChange(editorState) {
    this.props.onChange(editorState.getCurrentContent());
    this.setState({editorState});
  }

  render() {
    const {editorState} = this.state;
    return (
      <div style={styles.root}>
        <Editor editorState={editorState} onChange={this.onChange} />
      </div>
    );
  }
}

const styles = {
  root: {
    fontFamily: 'monospace',
  },
  fvar: {
    color: '#595BE4',
  },
  primitive: {
    color: '#159CF1',
  },
};
