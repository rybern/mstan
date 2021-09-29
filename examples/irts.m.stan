data {
  int I; // students
  int J; // questions
  array[I, J] int<lower=0, upper=1> y; // 1 if student i answered question j correctly
}

model {
  for(i in 1:I) {
    for(j in 1:J) {
      y[i, j] ~ bernoulli(IRT_Prob(i, j));
    }
  }
}

// "without-guesses" is one implementation of \`IRT_Prob\`
module "without-guesses" IRT_Prob(int i, int j) {
  return inv_logit(IRT_LogOdds(i, j));
}

module "no-discrimination" IRT_LogOdds(int i, int j) {
  return Intercept() + Ability(i) - Difficulty(j);
}

// Include an intercept in the IRT score
module "yes" Intercept() {
  parameters {
    real alpha;
  }
  return alpha;
}

// No intercept in the IRT score
module "no" Intercept() {
  return 0;
}

// Model each student's ability separately
module "unpooled" Ability(int i) {
  parameters {
    vector[I] beta;
  }
  return beta[i];
}

// Don't model separate ability (probably only makes sense with an intercept)
module "pooled" Ability(int i) {
  return 0;
}

// Model each item's difficulty separately
module "unpooled" Difficulty(int j) {
  parameters {
    vector[J] gamma;
  }
  return gamma[j];
}

module "pooled" Difficulty(int j) {
  return 0;
}

// Multiply the ability by the item's discrimination, like the Rasch model
module "ability-discrimination" IRT_LogOdds(int i, int j) {
  parameters {
    vector[J] delta;
  }
  return Intercept() + delta[j] * Ability(i) - Difficulty(j);
}

// Multiply the whole score by the item's discrimination, like the 3-PL model
module "total-discrimination" IRT_LogOdds(int i, int j) {
  parameters {
    vector[J] delta;
  }
  return delta[j] * (Intercept() + Ability(i) - Difficulty(j));
}

// Model an item's probability of being guessed
module "with-guesses" IRT_Prob(int i, int j) {
  parameters {
    vector[J] theta;
  }
  return theta[j] + (1 - theta[j]) * inv_logit(IRT_LogOdds(i, j));
}

/* Model #1
   Intercept: yes
   Ability: pooled
   Difficulty: pooled
   IRT_LogOdds: no-discrimination
   IRT_Prob: without-guesses
*/

/* Model #2A
   Intercept: yes
   Ability: unpooled
   Difficulty: pooled
   IRT_LogOdds: no-discrimination
   IRT_Prob: without-guesses
*/

/* Model #2B
   Intercept: no
   Ability: unpooled
   Difficulty: pooled
   IRT_LogOdds: no-discrimination
   IRT_Prob: without-guesses
*/

/* Model #3
   Intercept: no
   Ability: pooled
   Difficulty: unpooled
   IRT_LogOdds: no-discrimination
   IRT_Prob: without-guesses
*/

/* Model #4 "1-PL"
   Intercept: yes
   Ability: unpooled
   Difficulty: unpooled
   IRT_LogOdds: no-discrimination
   IRT_Prob: without-guesses
*/

/* Model #5 "Rasch"
   Intercept: yes
   Ability: unpooled
   Difficulty: pooled
   IRT_LogOdds: ability-discrimination
   IRT_Prob: without-guesses
*/

/* Model #6 "2-PL"
   Intercept: yes
   Ability: unpooled
   Difficulty: unpooled
   IRT_LogOdds: total-discrimination
   IRT_Prob: without-guesses
*/

/* Model #7 "3-PL"
   Intercept: yes
   Ability: unpooled
   Difficulty: unpooled
   IRT_LogOdds: total-discrimination
   IRT_Prob: with-guesses
*/
