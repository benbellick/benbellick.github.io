import React from 'react';
import { ASCIIArt, Terminal } from '../index.js';
import * as ascii from '../../assets/ASCIIArt/ascii.json';
import Typing from 'react-typing-animation';

const App = () => {
    var art = ascii.name.join('\n');
    return (
        <Terminal>
            <ASCIIArt>
                {art}
            </ASCIIArt>
        </Terminal>
    );
};

export default App;
