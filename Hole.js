import React from 'react';
import {
  CompositeDecorator,
  ContentState,
  Editor,
  EditorState,
} from 'draft-js';
import Autosuggest from 'react-autosuggest';

const computationHoles = {
  bvar: [],
  // hole: [],
  app: ['f', 'x'],
  annot: ['tm', 'ty'],
  'case': ['computation', 'type', 'branches'],
  choose: ['computation', 'access'],
  unpack: ['computation', 'usage', 'value', 'type'],
};

function getSuggestions(value) {
  const inputValue = value.trim().toLowerCase();
  const inputLength = inputValue.length;
  const names = Object.keys(computationHoles);

  return names.filter(name =>
    name.toLowerCase().slice(0, inputLength) === inputValue
  );
}

function getSuggestionValue(suggestion) {
  return suggestion;
}

function renderSuggestion(suggestion) {
  return (
    <span>{suggestion}</span>
  );
}

export default class Hole extends React.Component {
  constructor() {
    super();

    this.state = {
      value: '',
      suggestions: getSuggestions('')
    };

    this.onChange = (_evt, {newValue}) => this._onChange(newValue);
    this.onSuggestionsUpdateRequested = value =>
      this._onSuggestionsUpdateRequested(value);
    this.onSuggestionSelected = (_evt, {suggestionValue}) =>
      this.props.onSelect(suggestionValue);
  }

  _onChange(value) {
    this.setState({ value });
  }

  _onSuggestionsUpdateRequested({ value }) {
    this.setState({
      suggestions: getSuggestions(value)
    });
  }

  render() {
    const { value, suggestions } = this.state;
    const inputProps = {
      value,
      onChange: this.onChange,
    };

    return (
      <Autosuggest
        suggestions={suggestions}
        onSuggestionsUpdateRequested={this.onSuggestionsUpdateRequested}
        onSuggestionSelected={this.onSuggestionSelected}
        getSuggestionValue={getSuggestionValue}
        renderSuggestion={renderSuggestion}
        shouldRenderSuggestions={() => true}
        inputProps={inputProps}
      />
    );
  }
}
