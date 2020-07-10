from util import *

def find_matchings (problem, submission) :
  command = "../engine/main.native -cfg -solutions ../benchmarks_correct/" + problem + " -submission " + submission
  print (command)
  (out, err) = execute_command (command)
  if not err:
    #idx = out.find("# of matched solutions")
    loc = compute_loc (submission)
    out = "LOC : " + str(loc) + "\n" + out#out[idx:]
    print (out)
    return (command + "\n" + out)
  else:
  	return "\nError occurs : " + command