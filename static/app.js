// Common application utilities

async function apiCall(endpoint, options = {}) {
    const defaultOptions = {
        headers: {
            'Content-Type': 'application/json',
        },
        credentials: 'same-origin',
    };
    
    const mergedOptions = {
        ...defaultOptions,
        ...options,
        headers: {
            ...defaultOptions.headers,
            ...(options.headers || {}),
        },
    };
    
    try {
        const response = await fetch(endpoint, mergedOptions);
        const data = await response.json();
        return { response, data };
    } catch (error) {
        console.error('API call failed:', error);
        throw error;
    }
}

function checkAuth() {
    return apiCall('/api/user').then(({ response }) => {
        if (!response.ok) {
            window.location.href = '/login';
            return false;
        }
        return true;
    }).catch(() => {
        window.location.href = '/login';
        return false;
    });
}

// Logout functionality
document.addEventListener('DOMContentLoaded', () => {
    const logoutBtn = document.getElementById('logout');
    if (logoutBtn) {
        logoutBtn.addEventListener('click', async (e) => {
            e.preventDefault();
            try {
                await apiCall('/api/logout', { method: 'POST' });
                window.location.href = '/login';
            } catch (error) {
                console.error('Logout failed:', error);
            }
        });
    }
});

// MathJax rendering helper
function renderMath(element) {
    if (window.MathJax && window.MathJax.typesetPromise) {
        return MathJax.typesetPromise([element]).catch((err) => {
            console.error('MathJax rendering error:', err);
        });
    }
    return Promise.resolve();
}
