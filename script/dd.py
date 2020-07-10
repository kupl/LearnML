from util import *

def run_dd (problem, entry, submission, testcases, grading):
  opt_testcases = " -testcases " + testcases
  opt_entry = " -entry " + entry
  opt_submission = " -submission " + submission
  opt_solutions = " -solutions ../benchmarks_correct/" + problem
  if (grading == ""):
    opt_grading = ""
  else:
    opt_grading = " -grading " + grading
  command = "../engine/main.native -dd" + opt_submission + opt_solutions + opt_entry + opt_testcases + opt_grading
  print (command) 
  (out, err) = execute_command (command)
  if not err :
    print(out)
    return (command + "\n" + out)
  else:
    return "Error occurs : " + command