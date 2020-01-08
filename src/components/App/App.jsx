import React from 'react';
import { ASCIIArt, Terminal } from '../index.js';
import * as ascii from '../../assets/ASCIIArt/ascii.json';

const App = () => {
    var art = ascii.name.join('\n');
    return (
        <Terminal 
            windowName="benbellick@github.io"
            onExit = {() => console.log('Exit!')}
            onMinimize = {() => console.log('Minimize!')}
        >
            <ASCIIArt>
                {art}
            </ASCIIArt>
        </Terminal>
    );
};

export default App;
