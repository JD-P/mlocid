let currentCardId = null;
let editingCard = false;

document.addEventListener('DOMContentLoaded', async () => {
    await checkAuth();
    await loadCards();
    
    // Modal setup
    const cardModal = document.getElementById('cardModal');
    const importModal = document.getElementById('importModal');
    const newCardBtn = document.getElementById('newCardBtn');
    const importBtn = document.getElementById('importBtn');
    const cardForm = document.getElementById('cardForm');
    const importForm = document.getElementById('importForm');
    
    // Close modals when clicking X
    document.querySelectorAll('.close').forEach(closeBtn => {
        closeBtn.addEventListener('click', () => {
            cardModal.style.display = 'none';
            importModal.style.display = 'none';
        });
    });
    
    // Close modals when clicking outside
    window.addEventListener('click', (e) => {
        if (e.target === cardModal) cardModal.style.display = 'none';
        if (e.target === importModal) importModal.style.display = 'none';
    });
    
    // New card button
    newCardBtn.addEventListener('click', () => {
        editingCard = false;
        currentCardId = null;
        document.getElementById('modalTitle').textContent = 'New Card';
        document.getElementById('question').value = '';
        document.getElementById('answer').value = '';
        cardModal.style.display = 'block';
    });
    
    // Import button
    importBtn.addEventListener('click', () => {
        document.getElementById('importText').value = '';
        importModal.style.display = 'block';
    });
    
    // Cancel buttons
    document.getElementById('cancelCardBtn').addEventListener('click', () => {
        cardModal.style.display = 'none';
    });
    
    document.getElementById('cancelImportBtn').addEventListener('click', () => {
        importModal.style.display = 'none';
    });
    
    // Card form submission
    cardForm.addEventListener('submit', async (e) => {
        e.preventDefault();
        const question = document.getElementById('question').value;
        const answer = document.getElementById('answer').value;
        
        try {
            const endpoint = editingCard 
                ? `/api/flashcards/${currentCardId}`
                : '/api/flashcards';
            const method = editingCard ? 'PUT' : 'POST';
            
            const { response, data } = await apiCall(endpoint, {
                method,
                body: JSON.stringify({ question, answer }),
            });
            
            if (response.ok && data.success) {
                cardModal.style.display = 'none';
                await loadCards();
            } else {
                alert(data.error || 'Failed to save card');
            }
        } catch (error) {
            alert('Network error');
        }
    });
    
    // Import form submission
    importForm.addEventListener('submit', async (e) => {
        e.preventDefault();
        const importText = document.getElementById('importText').value;
        
        try {
            const response = await fetch('/api/import/mnemosyne', {
                method: 'POST',
                headers: {
                    'Content-Type': 'text/plain',
                },
                credentials: 'same-origin',
                body: importText,
            });
            const data = await response.json();
            
            if (response.ok && data.success) {
                alert(`Imported ${data.data.imported} cards successfully`);
                importModal.style.display = 'none';
                await loadCards();
            } else {
                alert(data.error || 'Failed to import cards');
            }
        } catch (error) {
            alert('Network error');
        }
    });
});

async function loadCards() {
    try {
        const { response, data } = await apiCall('/api/flashcards');
        
        if (response.ok && data.success) {
            const cardsList = document.getElementById('cardsList');
            const cards = data.data;
            
            if (cards.length === 0) {
                cardsList.innerHTML = '<p class="message">No cards yet. Create your first card!</p>';
                return;
            }
            
            cardsList.innerHTML = cards.map(card => `
                <div class="card-item">
                    <h3>Card #${card.id}</h3>
                    <div class="question"><strong>Question:</strong><br>${escapeHtml(card.question)}</div>
                    <div class="answer"><strong>Answer:</strong><br>${escapeHtml(card.answer)}</div>
                    <div class="meta">
                        E-Factor: ${card.efactor.toFixed(2)} | 
                        Interval: ${card.interval} days | 
                        Repetitions: ${card.repetitions}
                    </div>
                    <div class="actions">
                        <button class="edit-btn" onclick="editCard(${card.id})">Edit</button>
                        <button class="delete-btn" onclick="deleteCard(${card.id})">Delete</button>
                    </div>
                </div>
            `).join('');
            
            // Re-render MathJax for all cards
            if (window.MathJax && window.MathJax.typesetPromise) {
                await MathJax.typesetPromise();
            }
        }
    } catch (error) {
        console.error('Failed to load cards:', error);
    }
}

async function editCard(cardId) {
    try {
        const { response, data } = await apiCall(`/api/flashcards/${cardId}`);
        
        if (response.ok && data.success) {
            const card = data.data;
            editingCard = true;
            currentCardId = cardId;
            document.getElementById('modalTitle').textContent = 'Edit Card';
            document.getElementById('question').value = card.question;
            document.getElementById('answer').value = card.answer;
            document.getElementById('cardModal').style.display = 'block';
        }
    } catch (error) {
        alert('Failed to load card');
    }
}

async function deleteCard(cardId) {
    if (!confirm('Are you sure you want to delete this card?')) {
        return;
    }
    
    try {
        const { response, data } = await apiCall(`/api/flashcards/${cardId}`, {
            method: 'DELETE',
        });
        
        if (response.ok && data.success) {
            await loadCards();
        } else {
            alert(data.error || 'Failed to delete card');
        }
    } catch (error) {
        alert('Network error');
    }
}

function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}
