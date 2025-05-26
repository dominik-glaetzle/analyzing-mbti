from django.shortcuts import render
import subprocess
from django.conf import settings

def index(request):
    prediction = None

    if request.method == 'POST':
        user_input = request.POST.get('user_text')
        result = subprocess.run(
            ['Rscript', '/Users/dominik/coding/analyzing-mbti/R/mbti_predictor.r', user_input],
            capture_output=True,
            text=True
        )
        print("STDOUT:", result.stdout)
        print("STDERR:", result.stderr)
        prediction = result.stdout.strip()
        print(f"Prediction: {prediction}")
    return render(request, 'index.html', {"prediction": prediction})