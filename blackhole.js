document.addEventListener('DOMContentLoaded', () => {
    // Smooth scroll for navigation links
    document.querySelectorAll('#main-nav-bh a[href^="#"]').forEach(anchor => {
        anchor.addEventListener('click', function (e) {
            e.preventDefault();
            const targetId = this.getAttribute('href');
            const targetElement = document.querySelector(targetId);
            if (targetElement) {
                const offset = document.getElementById('main-nav-bh').offsetHeight;
                const bodyRect = document.body.getBoundingClientRect().top;
                const elementRect = targetElement.getBoundingClientRect().top;
                const elementPosition = elementRect - bodyRect;
                const offsetPosition = elementPosition - offset;

                window.scrollTo({
                    top: offsetPosition,
                    behavior: 'smooth'
                });
            }
        });
    });

    // Update footer year
    const yearSpanBH = document.getElementById('current-year-bh');
    if (yearSpanBH) {
        yearSpanBH.textContent = new Date().getFullYear();
    }

    // Interactive Black Hole Diagram Labels
    const diagramParts = [
        { selector: '.singularity-point', textId: 'singularity-text', label: "Singularity" },
        { selector: '.event-horizon-ring', textId: 'event-horizon-text', label: "Event Horizon" },
        { selector: '.photon-sphere-ring', textId: 'photon-sphere-text', label: "Photon Sphere" },
        { selector: '.ergosphere-visual', textId: 'ergosphere-text', label: "Ergosphere" }
        // Accretion disk and jets are visual, text is always there or could be added similarly
    ];
    const allLabelTexts = document.querySelectorAll('.diagram-labels .label-text');

    diagramParts.forEach(part => {
        const element = document.querySelector(part.selector);
        const textElement = document.getElementById(part.textId);

        if (element && textElement) {
            element.addEventListener('mouseenter', () => {
                allLabelTexts.forEach(txt => txt.classList.remove('active'));
                textElement.classList.add('active');
            });
            // No mouseleave to keep the last hovered item active until another is hovered
        }
    });

    // Initial active label (optional, e.g., event horizon)
    const initialActiveLabel = document.getElementById('event-horizon-text');
    if (initialActiveLabel) {
        initialActiveLabel.classList.add('active');
    }


    // Simple Parallax for background images on scroll (can be more sophisticated)
    // Ensure sections with 'parallax-bg' class also have an inline style for background-image
    window.addEventListener('scroll', () => {
        document.querySelectorAll('.parallax-bg').forEach(section => {
            let offset = window.pageYOffset;
            // Check if section is in viewport to optimize
            const rect = section.getBoundingClientRect();
            if (rect.bottom >= 0 && rect.top <= window.innerHeight) {
                section.style.backgroundPositionY = (offset - section.offsetTop) * 0.3 + "px";
            }
        });
    });

    // Add a few stars to the background using JS for dynamic placement
    const cosmicVoid = document.querySelector('.cosmic-void-background');
    if (cosmicVoid) {
        const numStars = 150; // Adjust number of stars
        for (let i = 0; i < numStars; i++) {
            let star = document.createElement('div');
            star.className = 'star';
            star.style.top = Math.random() * 100 + '%';
            star.style.left = Math.random() * 100 + '%';
            star.style.width = Math.random() * 2 + 1 + 'px'; // Star size
            star.style.height = star.style.width;
            star.style.animationDelay = Math.random() * 5 + 's'; // Randomize twinkle
            star.style.animationDuration = Math.random() * 5 + 3 + 's'; // Randomize twinkle speed
            cosmicVoid.appendChild(star);
        }
    }
});

// Add CSS for the JS-created stars in your style_blackhole.css or a <style> tag in HTML
/*
.star {
    position: absolute;
    background-color: white;
    border-radius: 50%;
    opacity: 0; // Start invisible, fade in with animation
    animation: twinkle 5s infinite ease-in-out;
    box-shadow: 0 0 3px #fff, 0 0 6px #fff; // Subtle glow
}

@keyframes twinkle {
    0%, 100% { opacity: 0; transform: scale(0.5); }
    50% { opacity: 0.8; transform: scale(1); }
}
*/