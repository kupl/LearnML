from util import *

def run_testcases (entry, submission, testcases, grading):
  opt_testcases = " -testcases " + testcases
  opt_entry = " -entry " + entry
  opt_submission = " -submission " + submission
  if (grading == ""):
    opt_grading = ""
  else:
    opt_grading = " -grading " + grading
  command = "../engine/main.native -run" + opt_submission + opt_entry + opt_testcases + opt_grading
  print (command) 
  (out, err) = execute_command (command)
  if not err :
    idx = out.find("score :")
    print (out[idx:])
    return (out[idx:])
  else:
    return out