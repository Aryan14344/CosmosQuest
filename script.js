document.addEventListener('DOMContentLoaded', () => {
    const questionTextElement = document.getElementById('question-text');
    const optionsContainer = document.getElementById('options-container');
    const feedbackTextElement = document.getElementById('feedback-text');
    const nextQuestionBtn = document.getElementById('next-question-btn');
    const restartBtn = document.getElementById('restart-btn');
    const quizArea = document.getElementById('quiz-area');
    const resultsArea = document.getElementById('results-area');
    const finalScoreElement = document.getElementById('final-score');
    const spaceFactTextElement = document.getElementById('space-fact-text');
    const questionCounterElement = document.getElementById('question-counter');
    const progressBarElement = document.getElementById('progress-bar');

    // Define all questions with difficulty levels
    // Difficulty 1: Easy, 2: Medium, 3: Hard
    const allQuestions = [
        // Difficulty 1
        { question: "What is the closest planet to the Sun?", options: ["Venus", "Mars", "Mercury", "Earth"], answer: "Mercury", difficulty: 1 },
        { question: "What is Earth's only natural satellite?", options: ["Phobos", "Moon", "Titan", "Europa"], answer: "Moon", difficulty: 1 },
        { question: "Which is the largest planet in our Solar System?", options: ["Saturn", "Jupiter", "Neptune", "Uranus"], answer: "Jupiter", difficulty: 1 },
        { question: "What is the name of our galaxy?", options: ["Andromeda", "Triangulum", "Whirlpool", "Milky Way"], answer: "Milky Way", difficulty: 1 },
        { question: "What is the Sun primarily composed of?", options: ["Oxygen & Nitrogen", "Rock & Metal", "Hydrogen & Helium", "Liquid & Gas"], answer: "Hydrogen & Helium", difficulty: 1},

        // Difficulty 2
        { question: "What force keeps planets in orbit around the Sun?", options: ["Magnetism", "Gravity", "Friction", "Dark Energy"], answer: "Gravity", difficulty: 2 },
        { question: "Which planet is known as the Red Planet?", options: ["Jupiter", "Mars", "Venus", "Saturn"], answer: "Mars", difficulty: 2 },
        { question: "What is the name of the first human-made object to reach space?", options: ["Vostok 1", "Apollo 11", "Sputnik 1", "Voyager 1"], answer: "Sputnik 1", difficulty: 2 },
        { question: "What is the hottest planet in our solar system?", options: ["Mercury", "Venus", "Mars", "Jupiter"], answer: "Venus", difficulty: 2 },
        { question: "How many probe sent by humans reached interstellar space ? ", options: ["1", "2", "0", "6"], answer: "8", difficulty: 2 },

        // Difficulty 3
        { question: "What is the boundary of a black hole called, beyond which nothing can escape?", options: ["Singularity", "Accretion Disk", "Event Horizon", "Photon Sphere"], answer: "Event Horizon", difficulty: 3 },
        { question: "What is the name of the predominant theory describing the origin of the universe?", options: ["Steady State", "Big Bang Theory", "String Theory", "Eternal Inflation"], answer: "Big Bang Theory", difficulty: 3 },
        { question: "Which space telescope, launched in 1990, has provided many iconic images of space?", options: ["James Webb", "Kepler", "Hubble", "Spitzer"], answer: "Hubble", difficulty: 3 },
        { question: "What type of star is our sun ?", options: ["Type O", "Type M", "Type F", "Type G"], answer: "Type G", difficulty: 3 },
        { question: "Orbit of which planet in Solar system can not be understood through newtons law of gravity", options: ["Uranus", "Mercury", "Earth", "Pluto"], answer: "Mercury", difficulty: 3 }
    ];

    let currentQuestionIndex = 0;
    let score = 0;
    let consecutiveCorrectAnswers = 0;
    let currentDifficulty = 1;
    let questionsForQuiz = []; // Will hold the 10 questions for this session
    let usedQuestionIndicesByDifficulty = { 1: [], 2: [], 3: [] }; // To avoid repeating questions from pools

    function shuffleArray(array) {
        for (let i = array.length - 1; i > 0; i--) {
            const j = Math.floor(Math.random() * (i + 1));
            [array[i], array[j]] = [array[j], array[i]];
        }
    }

    function selectQuestionsForQuiz() {
        questionsForQuiz = [];
        usedQuestionIndicesByDifficulty = { 1: [], 2: [], 3: [] };
        let tempCurrentDifficulty = 1;
        let tempConsecutiveCorrect = 0; // Simulate for question selection logic

        const difficultyPools = {
            1: allQuestions.filter(q => q.difficulty === 1),
            2: allQuestions.filter(q => q.difficulty === 2),
            3: allQuestions.filter(q => q.difficulty === 3)
        };
        // Shuffle each pool to get varied questions each time
        shuffleArray(difficultyPools[1]);
        shuffleArray(difficultyPools[2]);
        shuffleArray(difficultyPools[3]);

        for (let i = 0; i < 10; i++) {
            let questionPool = difficultyPools[tempCurrentDifficulty];
            
            // Fallback if pool is exhausted for current difficulty
            if (usedQuestionIndicesByDifficulty[tempCurrentDifficulty].length >= questionPool.length) {
                if (tempCurrentDifficulty < 3) { // Try harder
                    tempCurrentDifficulty++;
                    questionPool = difficultyPools[tempCurrentDifficulty];
                } else if (tempCurrentDifficulty > 1) { // Try easier
                    tempCurrentDifficulty--;
                    questionPool = difficultyPools[tempCurrentDifficulty];
                }
                // If still exhausted, pick from any remaining (this is a safety net)
                if (usedQuestionIndicesByDifficulty[tempCurrentDifficulty].length >= questionPool.length) {
                     const anyRemaining = allQuestions.filter(q => !questionsForQuiz.includes(q));
                     if (anyRemaining.length > 0) questionsForQuiz.push(anyRemaining[0]);
                     else console.error("Not enough unique questions available!"); // Should not happen with ample questions
                     continue;
                }
            }
            
            let questionToAdd = questionPool[usedQuestionIndicesByDifficulty[tempCurrentDifficulty].length];
            questionsForQuiz.push(questionToAdd);
            usedQuestionIndicesByDifficulty[tempCurrentDifficulty].push(questionToAdd); // Mark as used in this context

            // Simulate difficulty increase for *next* question selection logic
            // This part is tricky: difficulty increases based on *actual* answers.
            // So, question selection for the *quiz* is a bit different.
            // Let's simplify: select 3-4 easy, then 3-4 medium, then 2-3 hard initially.
            // The dynamic difficulty increase will happen during the quiz *play*.
            // For initial selection for the 10 questions:
            if (questionsForQuiz.length < 4) tempCurrentDifficulty = 1;
            else if (questionsForQuiz.length < 7) tempCurrentDifficulty = 2;
            else tempCurrentDifficulty = 3;
        }
        // The above selection is basic. The *actual* dynamic difficulty logic is applied during `loadQuestion` below.
        // For a true dynamic experience question-by-question:
        // We will pick questions ONE BY ONE during the quiz.
    }
    
    function getNextQuestion() {
        // Filter available questions for the currentDifficulty that haven't been used in `questionsForQuiz`
        let pool = allQuestions.filter(q => q.difficulty === currentDifficulty && !questionsForQuiz.includes(q));
        
        // Fallback logic if the current difficulty pool is exhausted
        if (pool.length === 0) {
            if (currentDifficulty < 3) { // Try next higher difficulty
                pool = allQuestions.filter(q => q.difficulty === currentDifficulty + 1 && !questionsForQuiz.includes(q));
            }
            if (pool.length === 0 && currentDifficulty > 1) { // Try next lower difficulty
                 pool = allQuestions.filter(q => q.difficulty === currentDifficulty - 1 && !questionsForQuiz.includes(q));
            }
            if (pool.length === 0) { // If still no questions, pick any unused
                pool = allQuestions.filter(q => !questionsForQuiz.includes(q));
            }
            if (pool.length === 0) { // Should not happen if there are 10+ unique questions
                console.error("Ran out of unique questions!");
                return null; 
            }
        }
        
        shuffleArray(pool); // Shuffle the chosen pool
        return pool[0]; // Pick the first one after shuffling
    }


    function loadQuestion() {
        feedbackTextElement.textContent = '';
        nextQuestionBtn.classList.add('hidden');
        optionsContainer.innerHTML = ''; // Clear previous options

        if (currentQuestionIndex >= 10) {
            showResults();
            return;
        }

        const questionData = getNextQuestion();
        if (!questionData) { // No more questions available
            showResults();
            return;
        }
        questionsForQuiz.push(questionData); // Add to the list of questions played in this session

        questionTextElement.textContent = questionData.question;
        questionCounterElement.textContent = `Question ${currentQuestionIndex + 1} of 10`;
        progressBarElement.style.width = `${((currentQuestionIndex + 1) / 10) * 100}%`;


        questionData.options.forEach(option => {
            const button = document.createElement('button');
            button.textContent = option;
            button.addEventListener('click', () => selectAnswer(option, questionData.answer, button));
            optionsContainer.appendChild(button);
        });
    }

    function selectAnswer(selectedOption, correctAnswer, button) {
        const allOptionButtons = optionsContainer.querySelectorAll('button');
        allOptionButtons.forEach(btn => btn.disabled = true); // Disable all buttons

        if (selectedOption === correctAnswer) {
            score++;
            consecutiveCorrectAnswers++;
            feedbackTextElement.textContent = 'Correct!';
            feedbackTextElement.style.color = '#28a745';
            button.classList.add('correct');
        } else {
            consecutiveCorrectAnswers = 0;
            feedbackTextElement.textContent = `Incorrect. The answer is ${correctAnswer}.`;
            feedbackTextElement.style.color = '#dc3545';
            button.classList.add('incorrect');
            // Highlight the correct answer
            allOptionButtons.forEach(btn => {
                if (btn.textContent === correctAnswer) {
                    btn.classList.add('correct');
                }
            });
        }

        // Difficulty adjustment logic
        if ((currentQuestionIndex + 1) % 3 === 0) { // After every 3 questions
            if (consecutiveCorrectAnswers >= 3) { // If all 3 were correct
                currentDifficulty = Math.min(3, currentDifficulty + 1); // Increase difficulty, max 3
                console.log("Difficulty increased to: " + currentDifficulty);
            }
            consecutiveCorrectAnswers = 0; // Reset streak for the next block of 3
        }
        
        nextQuestionBtn.classList.remove('hidden');
    }

    async function fetchSpaceFact() {
        spaceFactTextElement.textContent = "Fetching your cosmic insight...";
        try {
            // Assuming your Node.js server for Prolog is running on port 3000
            const response = await fetch('http://localhost:3000/get-fact');
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            const data = await response.json();
            spaceFactTextElement.textContent = data.fact || "Could not retrieve a space fact at this time.";
        } catch (error) {
            console.error("Error fetching space fact:", error);
            spaceFactTextElement.textContent = "Failed to fetch a space fact. Is the fact server running? (Check console for details)";
        }
    }

    function showResults() {
        quizArea.classList.add('hidden');
        resultsArea.classList.remove('hidden');
        finalScoreElement.textContent = score;
        fetchSpaceFact();
    }

    function startGame() {
        currentQuestionIndex = 0;
        score = 0;
        consecutiveCorrectAnswers = 0;
        currentDifficulty = 1; // Start with easy questions
        questionsForQuiz = []; // Reset for new game

        quizArea.classList.remove('hidden');
        resultsArea.classList.add('hidden');
        
        // selectQuestionsForQuiz(); // This was for pre-selecting, now we do it dynamically
        loadQuestion();
    }

    nextQuestionBtn.addEventListener('click', () => {
        currentQuestionIndex++;
        loadQuestion();
    });
    restartBtn.addEventListener('click', startGame);

    // Initial game start
    startGame();
});