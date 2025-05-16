document.addEventListener('DOMContentLoaded', () => {
    // Smooth scroll for navigation links
    document.querySelectorAll('a[href^="#"]').forEach(anchor => {
        anchor.addEventListener('click', function (e) {
            e.preventDefault();
            const targetId = this.getAttribute('href');
            const targetElement = document.querySelector(targetId);
            if (targetElement) {
                const offset = document.getElementById('main-nav').offsetHeight; // Adjust for sticky nav
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

    // Set dynamic background images for nebula type cards
    const nebulaTypeCards = document.querySelectorAll('.nebula-type-card');
    nebulaTypeCards.forEach(card => {
        const bgImage = card.getAttribute('data-bg-image');
        if (bgImage) {
            card.style.backgroundImage = `url('${bgImage}')`;
        }
    });

    // Parallax effect for sections (simple version)
    // More robust parallax might use requestAnimationFrame and more complex calculations
    const parallaxSections = document.querySelectorAll('.parallax-section');
    window.addEventListener('scroll', () => {
        parallaxSections.forEach(section => {
            const bgImage = section.getAttribute('data-bg');
            if (bgImage && !section.style.backgroundImage) { // Set only once
                 section.style.backgroundImage = `url('${bgImage}')`;
            }
            let offset = window.pageYOffset;
            // Adjust the '0.3' for more or less parallax effect
            // section.style.backgroundPositionY = (offset - section.offsetTop) * 0.3 + "px";
        });
    });
    // Initial call to set parallax backgrounds on load
    parallaxSections.forEach(section => {
        const bgImage = section.getAttribute('data-bg');
        if (bgImage) {
             section.style.backgroundImage = `url('${bgImage}')`;
        }
    });


    // Lightbox Gallery Functionality
    const galleryImages = document.querySelectorAll('.gallery-image-wrapper');
    const lightbox = document.getElementById('lightbox-modal');
    const lightboxImage = document.getElementById('lightbox-image');
    const lightboxTitle = document.getElementById('lightbox-title');
    const lightboxDescription = document.getElementById('lightbox-description');
    const closeLightboxBtn = document.querySelector('.close-lightbox');

    galleryImages.forEach(item => {
        item.addEventListener('click', () => {
            const imgSrc = item.querySelector('img').src;
            const title = item.getAttribute('data-title');
            const description = item.getAttribute('data-description');

            lightboxImage.src = imgSrc;
            lightboxTitle.textContent = title || '';
            lightboxDescription.textContent = description || '';
            lightbox.style.display = 'block';
            document.body.style.overflow = 'hidden'; // Prevent scrolling background
        });
    });

    if (closeLightboxBtn) {
        closeLightboxBtn.addEventListener('click', () => {
            lightbox.style.display = 'none';
            document.body.style.overflow = 'auto';
        });
    }

    // Close lightbox when clicking outside the image
    if (lightbox) {
        lightbox.addEventListener('click', (e) => {
            if (e.target === lightbox) { // Clicked on the dark background
                lightbox.style.display = 'none';
                document.body.style.overflow = 'auto';
            }
        });
    }

    // Update footer year
    const yearSpan = document.getElementById('current-year');
    if (yearSpan) {
        yearSpan.textContent = new Date().getFullYear();
    }

    // Sticky Nav Hiding/Showing on scroll (optional)
    let lastScrollTop = 0;
    const mainNav = document.getElementById('main-nav');
    window.addEventListener("scroll", function(){
       let currentScroll = window.pageYOffset || document.documentElement.scrollTop;
       if (currentScroll > lastScrollTop && currentScroll > mainNav.offsetHeight + 50){ // Scroll Down and past nav height
         mainNav.style.top = `-${mainNav.offsetHeight + 5}px`; // Hide nav
       } else {
         mainNav.style.top = "0"; // Show nav
       }
       lastScrollTop = currentScroll <= 0 ? 0 : currentScroll; // For Mobile or negative scrolling
    }, false);

});