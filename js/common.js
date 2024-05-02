// Vim scrolling bindings
document.addEventListener('keydown', function(e) {
    switch (e.key) {
        case 'j':
            window.scrollBy(0, 20);
            break;
        case 'k':
            window.scrollBy(0, -20);
            break;
        case 'J':
            window.scrollBy(0, window.innerHeight / 2);
            break;
        case 'K':
            window.scrollBy(0, -window.innerHeight / 2);
            break;
    }
});
