import React from 'react';
import { ASCIIArt, Terminal } from '../index.js';
import * as ascii from '../../assets/ASCIIArt/ascii.json';

const App = () => {
    console.log(ascii.name[1]);
    return (
        <Terminal>
            <ASCIIArt>
                {ascii.name}
            </ASCIIArt>
        </Terminal>
    );
};

export default App;
