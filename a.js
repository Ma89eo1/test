const canvas = document.createElement('canvas');
canvas.style.position = 'fixed';
canvas.style.top = '0';
canvas.style.left = '0';
canvas.style.width = '100%';
canvas.style.height = '100vh';
canvas.style.background = '#000';
document.body.appendChild(canvas);
const ctx = canvas.getContext('2d');
canvas.width = window.innerWidth;
canvas.height = window.innerHeight;

const planets = [];
const planetCount = 8;
let time = 0;

class Planet {
  constructor(index) {
    this.angle = Math.random() * Math.PI * 2;
    this.radius = 50 + index * 30;
    this.speed = 0.02 / (index + 1);
    this.size = 5 + Math.random() * 5;
    this.hue = Math.random() * 360;
  }
  update() {
    this.angle += this.speed;
    this.x = canvas.width / 2 + Math.cos(this.angle) * this.radius;
    this.y = canvas.height / 2 + Math.sin(this.angle) * this.radius;
  }
  draw() {
    ctx.beginPath();
    ctx.arc(this.x, this.y, this.size, 0, Math.PI * 2);
    ctx.fillStyle = `hsl(${this.hue + time * 15 % 360}, 80%, 60%)`;
    ctx.fill();
    ctx.beginPath();
    ctx.arc(canvas.width / 2, canvas.height / 2, this.radius, 0, Math.PI * 2);
    ctx.strokeStyle = `hsla(${this.hue + time * 15 % 360}, 80%, 60%, 0.2)`;
    ctx.lineWidth = 1;
    ctx.stroke();
  }
}

for (let i = 0; i < planetCount; i++) {
  planets.push(new Planet(i));
}

function animate() {
  ctx.fillStyle = 'rgba(0, 0, 0, 0.1)';
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  planets.forEach(planet => {
    planet.update();
    planet.draw();
  });
  time += 0.05;
  requestAnimationFrame(animate);
}

animate();
