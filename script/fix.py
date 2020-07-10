from util import *

def run_fix (problem, entry, submission, testcases, grading):
  opt_testcases = " -testcases " + testcases
  opt_entry = " -entry " + entry
  opt_submission = " -submission " + submission
  opt_solution = " -solution ../benchmarks_correct/" + problem + "/sol.ml"
  if (grading == ""):
    opt_grading = ""
  else:
    opt_grading = " -grading " + grading
  command = "../engine/main.native -fix" + opt_submission + opt_solution + opt_entry + opt_testcases + opt_grading
  print (command) 
  (out, err) = execute_command (command)
  if not err :
    print(out)
    return (command + "\n" + out)
  else:
    return "Error occurs : " + command