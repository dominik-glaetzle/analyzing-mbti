const slogans = [
  "Analyze your personality.",
  "Understand your MBTI type.",
  "Discover what your words say about your personality.",
];

let sloganElement;
let currentIndex = 0;
let charIndex = 0;
let isDeleting = false;

function typeSlogan() {
  const currentText = slogans[currentIndex];
  const visibleText = currentText.substring(0, charIndex);
  sloganElement.textContent = visibleText || "\u00A0";

  if (!isDeleting && charIndex < currentText.length) {
    charIndex++;
    setTimeout(typeSlogan, 100);
  } else if (isDeleting && charIndex > 0) {
    charIndex--;
    setTimeout(typeSlogan, 50);
  } else {
    isDeleting = !isDeleting;
    if (!isDeleting) currentIndex = (currentIndex + 1) % slogans.length;
    setTimeout(typeSlogan, 1000);
  }
}

document.addEventListener("DOMContentLoaded", () => {
  sloganElement = document.getElementById("slogan-text");
  sloganElement.style.visibility = "visible";
  typeSlogan();
});
