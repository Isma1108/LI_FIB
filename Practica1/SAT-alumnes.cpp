#include <iostream>
#include <stdlib.h>
#include <vector>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0

uint numVars;
uint numClauses;
vector<vector<int>> clauses;
vector<int> model;
vector<int> modelStack;
uint indexOfNextLitToPropagate;
uint decisionLevel;

//Vectores de ocurrencias para acelerar la propagación.
//En la posicion i-esima se encuentra el vector de clausulas
//donde la variable i-esima tiene un valor positivo o negativo, respectivamente
vector<vector<int>> positiveOcurrList;
vector<vector<int>> negativeOcurrList;

//en la posición i-esima se encuentra el numero de conflictos en los que
//aparece la variable i-esima
vector<int> conflicts;

//en la posicion i-esima se encuentra las veces que aparece el literal
//i-esimo
vector<int> ocurrList;

//Frecuencia de reduccion de conflictos (cada 10000)
const int REDUCE_CONFL_FREQ = 10000;

//Factor de disminucion de conflictos
const double DECAY_FACTOR = 0.75;

//Numero de conflictos
int numConflicts = 0;


void readClauses( ){
  // Skip comments
  char c = cin.get();
  while (c == 'c') {
    while (c != '\n') c = cin.get();
    c = cin.get();
  }  
  // Read "cnf numVars numClauses"
  string aux;
  cin >> aux >> numVars >> numClauses;
  clauses.resize(numClauses);  
  positiveOcurrList.resize(numVars + 1);
  negativeOcurrList.resize(numVars + 1);
  conflicts.resize(numVars + 1, 0);
  ocurrList.resize(numVars + 1, 0);

  // Read clauses
  for (uint i = 0; i < numClauses; ++i) {
    int lit;
    while (cin >> lit and lit != 0) {
      clauses[i].emplace_back(lit);
      ++ocurrList[abs(lit)];
      if (lit > 0) positiveOcurrList[lit].emplace_back(i);
      else negativeOcurrList[-lit].emplace_back(i);
    }
  }    
}


int currentValueInModel(int lit){
  if (lit >= 0) return model[lit];
  else {
    if (model[-lit] == UNDEF) return UNDEF;
    else return 1 - model[-lit];
  }
}


void setLiteralToTrue(int lit){
  modelStack.push_back(lit);
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;		
}

void update_conflicts() {
  for (uint i = 1; i <= numVars; ++i) conflicts[i] *= DECAY_FACTOR;
}


//para hacer mas eficiente la propagacion:
//en vez de propagar recorriendo todas las clausulas,
//recorremos solo aquellas en las que el literal aparece de
//forma que evalua a falso.
//Si es positivo -> miramos solo las clausulas donde aparece negado
//Si es negativo -> miramos solo las clausulas donde aparece sin negar

bool propagateGivesConflict ( ) {
  while (indexOfNextLitToPropagate < modelStack.size() ) {
    if (numConflicts % REDUCE_CONFL_FREQ == 0) update_conflicts();

    int litToPropagate = modelStack[indexOfNextLitToPropagate];
    ++indexOfNextLitToPropagate;
  
    if (litToPropagate > 0) {
      for (uint i = 0; i < negativeOcurrList[litToPropagate].size(); ++i) {
        int idxClause = negativeOcurrList[litToPropagate][i];
        bool someLitTrue = false;
        int numUndefs = 0;
        int lastLitUndef = 0;
        for (uint k = 0; not someLitTrue and k < clauses[idxClause].size(); ++k) {
	        int val = currentValueInModel(clauses[idxClause][k]);
	        if (val == TRUE) someLitTrue = true;
	        else if (val == UNDEF){ ++numUndefs; lastLitUndef = clauses[idxClause][k]; }
        }
        if (not someLitTrue and numUndefs == 0) {
          ++numConflicts;
          for (uint k = 0; not someLitTrue and k < clauses[idxClause].size(); ++k) {
            int litConfl = clauses[idxClause][k];
            ++conflicts[abs(litConfl)];
          }
          return true; // conflict! all lits false
        }
        else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);	
      }
    }
    else {
      for (uint i = 0; i < positiveOcurrList[-litToPropagate].size(); ++i) {
        int idxClause = positiveOcurrList[-litToPropagate][i];
        bool someLitTrue = false;
        int numUndefs = 0;
        int lastLitUndef = 0;
        for (uint k = 0; not someLitTrue and k < clauses[idxClause].size(); ++k) {
	        int val = currentValueInModel(clauses[idxClause][k]);
	        if (val == TRUE) someLitTrue = true;
	        else if (val == UNDEF){ ++numUndefs; lastLitUndef = clauses[idxClause][k]; }
        }
        if (not someLitTrue and numUndefs == 0) {
          ++numConflicts;
          for (uint k = 0; not someLitTrue and k < clauses[idxClause].size(); ++k) {
            int litConfl = clauses[idxClause][k];
            ++conflicts[abs(litConfl)];
          }
          return true; // conflict! all lits false
        }
        else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);	
      }
    }
  }
  return false;
}


void backtrack(){
  uint i = modelStack.size() -1;
  int lit = 0;
  while (modelStack[i] != 0){ // 0 is the DL mark
    lit = modelStack[i];
    model[abs(lit)] = UNDEF;
    modelStack.pop_back();
    --i;
  }
  // at this point, lit is the last decision
  modelStack.pop_back(); // remove the DL mark
  --decisionLevel;
  indexOfNextLitToPropagate = modelStack.size();
  setLiteralToTrue(-lit);  // reverse last decision
}


// Heuristic for finding the next decision literal:
int getNextDecisionLiteral(){
  int decLit = -1;
  float bestHeuristicValue = -1.0;

  for (uint i = 1; i <= numVars; ++i) {
    if (model[i] == UNDEF) {
      float hValue = 0.70 * conflicts[i] + 0.30 * ocurrList[i];
      if (hValue > bestHeuristicValue) {
        bestHeuristicValue = hValue;
        decLit = i;
      }
    }
  }
  return (decLit != -1 ? decLit : 0);
}

void checkmodel(){
  for (uint i = 0; i < numClauses; ++i){
    bool someTrue = false;
    for (uint j = 0; not someTrue and j < clauses[i].size(); ++j)
      someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
    if (not someTrue) {
      cout << "Error in model, clause is not satisfied:";
      for (uint j = 0; j < clauses[i].size(); ++j) cout << clauses[i][j] << " ";
      cout << endl;
      exit(1);
    }
  }  
}

int main(){ 
  readClauses(); // reads numVars, numClauses and clauses
  model.resize(numVars+1,UNDEF);
  indexOfNextLitToPropagate = 0;  
  decisionLevel = 0;
  
  // Take care of initial unit clauses, if any
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
      else if (val == UNDEF) setLiteralToTrue(lit);
    }
  
  // DPLL algorithm (Davis-Putnam-Logemann-Loveland)
  while (true) {
    while (propagateGivesConflict()) {
      if ( decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; return 10; }
      backtrack();
    }
    int decisionLit = getNextDecisionLiteral();
    if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; return 20; }
    // start new decision level:
    modelStack.push_back(0);  // push mark indicating new DL
    ++indexOfNextLitToPropagate;
    ++decisionLevel;
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}  
