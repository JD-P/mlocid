let dueCards = [];
let currentCardIndex = 0;
let answerShown = false;

document.addEventListener('DOMContentLoaded', async () => {
    await checkAuth();
    await loadDueCards();
});

async function loadDueCards() {
    try {
        const { response, data } = await apiCall('/api/study/due');
        
        if (response.ok && data.success) {
            dueCards = data.data;
            
            if (dueCards.length === 0) {
                document.getElementById('noCards').style.display = 'block';
                document.getElementById('cardView').style.display = 'none';
            } else {
                document.getElementById('noCards').style.display = 'none';
                document.getElementById('cardView').style.display = 'block';
                showCard(0);
            }
        }
    } catch (error) {
        console.error('Failed to load due cards:', error);
    }
}

function showCard(index) {
    if (index >= dueCards.length) {
        document.getElementById('noCards').style.display = 'block';
        document.getElementById('cardView').style.display = 'none';
        return;
    }
    
    currentCardIndex = index;
    answerShown = false;
    const card = dueCards[index];
    
    document.getElementById('cardProgress').textContent = 
        `Card ${index + 1} of ${dueCards.length}`;
    document.getElementById('questionText').textContent = card.question;
    document.getElementById('answerText').textContent = card.answer;
    
    document.getElementById('cardFront').style.display = 'block';
    document.getElementById('cardBack').style.display = 'none';
    
    // Re-render MathJax
    if (window.MathJax && window.MathJax.typesetPromise) {
        MathJax.typesetPromise([document.getElementById('questionText')]).catch(err => {
            console.error('MathJax error:', err);
        });
    }
    
    // Setup quality buttons
    document.querySelectorAll('.quality-btn').forEach(btn => {
        btn.onclick = () => submitQuality(parseInt(btn.dataset.quality));
    });
}

document.getElementById('showAnswerBtn').addEventListener('click', () => {
    answerShown = true;
    document.getElementById('cardFront').style.display = 'none';
    document.getElementById('cardBack').style.display = 'block';
    
    // Re-render MathJax for answer
    if (window.MathJax && window.MathJax.typesetPromise) {
        MathJax.typesetPromise([document.getElementById('answerText')]).catch(err => {
            console.error('MathJax error:', err);
        });
    }
});

async function submitQuality(quality) {
    const card = dueCards[currentCardIndex];
    
    try {
        const { response, data } = await apiCall(`/api/study/review/${card.id}`, {
            method: 'POST',
            body: JSON.stringify({ quality }),
        });
        
        if (response.ok && data.success) {
            // Remove the reviewed card from the list
            dueCards.splice(currentCardIndex, 1);
            
            // Show next card
            if (currentCardIndex >= dueCards.length) {
                currentCardIndex = dueCards.length - 1;
            }
            
            if (dueCards.length === 0) {
                document.getElementById('noCards').style.display = 'block';
                document.getElementById('cardView').style.display = 'none';
            } else {
                showCard(currentCardIndex);
            }
        } else {
            alert(data.error || 'Failed to submit review');
        }
    } catch (error) {
        alert('Network error');
    }
}
