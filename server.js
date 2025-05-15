// server.js
const express = require('express');
const { exec } = require('child_process');
const path = require('path');
const cors = require('cors'); // Import cors

const app = express();
const port = process.env.PORT || 3000;

// Use CORS middleware
app.use(cors()); // This will allow requests from any origin

app.get('/get-fact', (req, res) => {
    // Ensure SWI-Prolog ('swipl') is in your system's PATH
    // or provide the full path to the executable.
    // Adjust the path to 'facts.pl' if it's not in the same directory as server.js
    const prologScriptPath = path.join(__dirname, 'facts.pl');
    
    // The command to execute:
    // -s loads the script
    // -g executes the goal
    // writeln(Fact) prints the fact to standard output
    // halt. exits Prolog
    // Make sure paths with spaces are quoted if necessary (though path.join handles this well for the script path)
    const command = `swipl -s "${prologScriptPath}" -g "get_random_fact(Fact), format('~w', [Fact]), halt."`;
    // Using format/2 instead of writeln to avoid extra quotes Prolog might add for atoms containing spaces.

    exec(command, (error, stdout, stderr) => {
        if (error) {
            console.error(`Prolog execution error: ${error.message}`);
            console.error(`Prolog stderr: ${stderr}`);
            return res.status(500).json({ error: 'Failed to execute Prolog script', details: stderr || error.message });
        }
        if (stderr) {
            console.warn(`Prolog stderr output: ${stderr}`); // Some Prolog versions might output info messages to stderr
        }
        // stdout should contain the fact. Trim any extra whitespace.
        const fact = stdout.trim();
        if (fact) {
            res.json({ fact: fact });
        } else {
            console.error('Prolog script did not output a fact.');
            res.status(500).json({ error: 'Prolog script did not return a fact.' });
        }
    });
});

app.listen(port, () => {
    console.log(`CosmoQuest Fact Server listening at http://localhost:${port}`);
    console.log("Ensure SWI-Prolog (swipl) is installed and in your system's PATH.");
    console.log("Make sure 'facts.pl' is in the same directory as this server.");
});