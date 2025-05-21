import React, { useState, useEffect } from 'react';
import { motion } from 'framer-motion';
import { Button, Paper, Typography, Box, CircularProgress } from '@mui/material';
import QuestionArea from './components/QuestionArea';
import CharacterDisplay from './components/CharacterDisplay';
import GameControls from './components/GameControls';

function App() {
  const [currentQuestion, setCurrentQuestion] = useState(null);
  const [gameState, setGameState] = useState({
    isLoading: false,
    solution: null,
    remainingCharacters: 25,
  });

  const fetchQuestion = async () => {
    setGameState(prev => ({ ...prev, isLoading: true }));
    try {
      const response = await fetch('/guess', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({}),
      });
      const data = await response.json();
      processResponse(data);
    } catch (error) {
      console.error('Error:', error);
    } finally {
      setGameState(prev => ({ ...prev, isLoading: false }));
    }
  };

  const handleAnswer = async (answer) => {
    if (!currentQuestion) return;
    
    setGameState(prev => ({ ...prev, isLoading: true }));
    try {
      const response = await fetch('/guess', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          attribute: currentQuestion,
          answer: answer
        }),
      });
      const data = await response.json();
      processResponse(data);
    } catch (error) {
      console.error('Error:', error);
    } finally {
      setGameState(prev => ({ ...prev, isLoading: false }));
    }
  };

  const processResponse = (data) => {
    if (data.gameOver) {
      setGameState(prev => ({ ...prev, solution: data.solution }));
      setCurrentQuestion(null);
    } else {
      setCurrentQuestion(data.nextQuestion);
      setGameState(prev => ({ 
        ...prev, 
        remainingCharacters: data.remainingCharacters || prev.remainingCharacters 
      }));
    }
  };

  const resetGame = async () => {
    try {
      await fetch('/reset');
      setGameState({
        isLoading: false,
        solution: null,
        remainingCharacters: 25,
      });
      fetchQuestion();
    } catch (error) {
      console.error('Error:', error);
    }
  };

  useEffect(() => {
    fetchQuestion();
  }, []);

  return (
    <motion.div
      initial={{ opacity: 0 }}
      animate={{ opacity: 1 }}
      className="min-h-screen bg-gradient-to-br from-purple-500 to-pink-500 py-8 px-4"
    >
      <Paper 
        elevation={8}
        className="max-w-2xl mx-auto p-8 rounded-2xl"
      >
        <Typography variant="h2" className="text-center mb-8 text-purple-700 font-bold">
          Adivina Quién
        </Typography>

        <CharacterDisplay 
          solution={gameState.solution}
          isLoading={gameState.isLoading}
        />

        <QuestionArea 
          currentQuestion={currentQuestion}
          isLoading={gameState.isLoading}
          remainingCharacters={gameState.remainingCharacters}
        />

        <GameControls 
          onYes={() => handleAnswer('yes')}
          onNo={() => handleAnswer('no')}
          onReset={resetGame}
          disabled={gameState.isLoading || !currentQuestion}
        />

        {gameState.isLoading && (
          <Box display="flex" justifyContent="center" mt={4}>
            <CircularProgress />
          </Box>
        )}
      </Paper>
    </motion.div>
  );
}

export default App;