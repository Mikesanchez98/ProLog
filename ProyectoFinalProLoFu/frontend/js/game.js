class AdivinaQuienGame {
    constructor() {
        this.currentQuestion = null;
        this.initializeElements();
        this.setupEventListeners();
        this.fetchQuestion();
    }

    initializeElements() {
        this.elements = {
            questionArea: document.getElementById('question-area'),
            yesBtn: document.getElementById('yes-btn'),
            noBtn: document.getElementById('no-btn'),
            resetBtn: document.getElementById('reset-btn'),
            charImage: document.getElementById('char-img'),
            progress: document.getElementById('progress')
        };
    }

    setupEventListeners() {
        this.elements.yesBtn.addEventListener('click', () => this.handleAnswer('yes'));
        this.elements.noBtn.addEventListener('click', () => this.handleAnswer('no'));
        this.elements.resetBtn.addEventListener('click', () => this.resetGame());
    }

    async fetchQuestion() {
        try {
            const response = await fetch('/guess', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({})
            });
            
            const data = await response.json();
            this.processResponse(data);
        } catch (error) {
            console.error('Error:', error);
        }
    }

    async handleAnswer(answer) {
        if (!this.currentQuestion) return;
        
        try {
            const response = await fetch('/guess', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    attribute: this.currentQuestion,
                    answer: answer
                })
            });
            
            const data = await response.json();
            this.processResponse(data);
        } catch (error) {
            console.error('Error:', error);
        }
    }

    processResponse(data) {
        if (data.gameOver) {
            this.showSolution(data.solution);
        } else {
            this.showQuestion(data.nextQuestion);
        }
    }

    showQuestion(question) {
        this.currentQuestion = question;
        this.elements.questionArea.textContent = this.formatQuestion(question);
        this.updateProgress();
    }

    showSolution(character) {
        this.currentQuestion = null;
        this.elements.questionArea.textContent = `¡Es ${character}!`;
        this.elements.charImage.src = `images/${character}.jpg`;
        this.elements.charImage.style.display = 'block';
        this.elements.progress.textContent = '¡Lo adiviné!';
    }

    async resetGame() {
        try {
            await fetch('/reset');
            this.elements.charImage.style.display = 'none';
            this.elements.progress.textContent = '';
            this.fetchQuestion();
        } catch (error) {
            console.error('Error:', error);
        }
    }

    formatQuestion(attr) {
        // Simple formateo - mejorar con reglas de personajes.pl
        const attrText = attr.replace(/_/g, ' ');
        return `¿Tu personaje ${attrText}?`;
    }

    updateProgress() {
        // Podría implementarse con conteo de personajes posibles
        this.elements.progress.textContent = 'Pensando...';
    }
}

// Iniciar el juego cuando el DOM esté listo
document.addEventListener('DOMContentLoaded', () => {
    new AdivinaQuienGame();
});