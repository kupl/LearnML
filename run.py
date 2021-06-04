import os
import subprocess
import argparse
import datetime

################################################################################
# All problems represented in the paper's main table (problem, entry function) #
################################################################################
BENCHMARK = {  
  ("max", "max"),
  ("mem", "mem"),
  ("mirror", "mirror"),
  ("sigma", "sigma"),
  ("iter", "iter"),
  ("uniq", "uniq"),
  ("nat", "natmul"),
  ("formula", "eval"),
  ("lambda", "check"),  
  ("diff", "grading")
}

# Benchmarks' directiory
BENCHMARK_DIR = "benchmarks"
TA_DIR = os.path.join(BENCHMARK_DIR, "ta_solutions")
TEST_DIR = os.path.join(BENCHMARK_DIR, "testcases")
CORRECT_DIR = os.path.join(BENCHMARK_DIR, "C")
INCORRECT_DIR = os.path.join(BENCHMARK_DIR, "I")

############################################################
#           Options for running this script                #
############################################################
def parsing_arguments():
  parser = argparse.ArgumentParser(description = "This script is written for reproducing tables in the paper")

  parser.add_argument("-option", type = str, default = "all",
    choices = ["all", "cafe", "fixml", "func", "prog"],
    help = "Input an option of [all, cafe, fixml, func, prog]\n the default option is \'all\'"
    )

  parser.add_argument("-problem", type = str, default = None,
    choices = ["max", "mem", "mirror", "sigma", "iter", "uniq", "nat", "formula", "lambda", "diff"],
    help = "Input a name of problem you want to test (e.g., mirror)"
    )

  args = parser.parse_args()
  return (args.option, args.problem)

##################################################################
#  The subfunctions for utility. e.g., find directory lists etc. #
##################################################################
# find the all path of subdirectories
def find_dir(path):
  whole_list = os.listdir(path)
  result = []
  for name in whole_list :
    full_path = os.path.join(path,name)
    if(os.path.isdir(full_path)):
      result.append(full_path)
    else:
      continue
  return result

# find the all path of subfiles
def find_files(path):
  whole_list = os.listdir(path)
  result = []
  for f in whole_list :
    result.append(os.path.join(path,f))
  return result

# make directory for logging
def make_dir(directory):
  if not os.path.exists(directory):
    os.makedirs(directory)

# Executing the given shell commands
def execute_command(command):
  p = subprocess.Popen(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,shell=True)
  try: 
    out, err = p.communicate (timeout=70)
    out = out.decode()
    return (out, err)
  except subprocess.TimeoutExpired:
    p.terminate() 
    p.wait()
    return ("", True)
    
#############################################
#  Functions for stroing evaluation results #
#############################################

# Create a directory for storing result : result:hh::mm:ss/opt/problem/sub -> (original, patch, result) #
def make_result_dir(result_dir, problem, submission, opt):
  # result_time/opt
  result_dir = os.path.join(result_dir, opt)
  make_dir(result_dir)

  # result_time/opt/problem
  result_dir = os.path.join(result_dir, problem)
  make_dir(result_dir)

  # result_time/opt/problem/sub_n/
  result_dir = os.path.join(result_dir, submission)
  make_dir(result_dir)

  return result_dir

# Parsing the original program, output, result (time, score, examples, etc...)
def parse_result(out):
  result_list = out.split("-----------------------------")
  
  original = result_list[2]
  patch = result_list[4]
  result = result_list[6]

  return (original.strip(), patch.strip(), result.strip())

###############################################
# Functions for running script with an option #
###############################################
def run_opt_with_submission (result_dir, problem, entry, submission_path, opt):
  submission_name = submission_path.split('/')[-1].replace(".ml", "")
  
  # Generate command
  opt_submission = " -submission " + submission_path
  opt_entry = " -entry " + entry
  opt_testcases = " -testcases " + os.path.join(TEST_DIR, problem + "_testcases")

  if opt == "fixml":
    opt_solution = " -solution " + os.path.join(TA_DIR, problem + "_solution.ml")
  else:
    opt_solution = " -solutions " + os.path.join(CORRECT_DIR, problem)

  if opt == "fixml" or opt == "cafe":
    opt_run = " -fix"
  elif opt == "prog":
    opt_run = " -prog"
  elif opt == "func":
    opt_run = " -func"

  # If a test driver is required, provide driver 
  test_driver = os.path.join(TEST_DIR, problem + "_grading.ml")
  if os.path.exists(test_driver):
    opt_grading = " -grading " + test_driver
  else:
    opt_grading = ""

  command = "engine/main.native" + opt_run + opt_submission + opt_solution + opt_testcases + opt_grading + opt_entry
  print(command)
  (out, err) = execute_command(command)
  result_dir = make_result_dir(result_dir, problem, submission_name, opt)
  print(out)

  if not err:
    (original, patch, result) = parse_result(out)

    # Save original submission 
    temp_original = os.path.join(result_dir, "temp_original")
    with open (temp_original, "w") as temp_file:
      temp_file.write(original)
      
    with open (temp_original, "r") as temp_file:
      original_path = os.path.join(result_dir, "original.ml")
      (out, err) = execute_command("ocamlformat " + temp_original + " -o " + original_path)
      if err:
        execute_command("cp " + temp_original + " " + original_path)
    execute_command ("rm " + temp_original)
    
    # Save patch result
    temp_patch = os.path.join(result_dir, "temp_patch")
    with open (temp_patch, "w") as temp_file:
      temp_file.write(patch)

    with open (temp_patch, "r") as temp_file:
      patch_path = os.path.join(result_dir, "patch.ml")
      (out, err) = execute_command("ocamlformat " + temp_patch + " -o " + patch_path)
      if err:
        execute_command("echo \"" + patch + "\" > " + patch_path)
    execute_command ("rm " + temp_patch)

    with open (os.path.join(result_dir, "result.txt"), "w") as result_file:
      result_file.write(result)
  else:
    execute_command ("ocamlformat " + submission_path + " -o " + os.path.join(result_dir, "original.ml"))
    execute_command ("echo \";;\nNone\n\" > " + os.path.join(result_dir, "patch.ml"))
    execute_command ("echo \"[Timeout]: fails to run the submission " + submission_path + "\" > " + os.path.join(result_dir, "result.txt"))


def run_opt_with_problem (result_dir, problem, opt):
  print ("[INFO]: run problem \'" + problem + "\' by \'" + opt + "\' option")
  for (problem_name, entry) in BENCHMARK:
    if problem == problem_name:
      submissions_path = os.path.join(INCORRECT_DIR, problem)
      submissions = find_files(submissions_path)
      submissions.sort(key = lambda x : int(x.split("/")[-1].split("sub")[1].split(".")[0]))
      for submission in submissions:
        run_opt_with_submission(result_dir, problem, entry, submission, opt)
    else:
      pass

def run_opt_all (result_dir, opt):
  for (problem, entry) in BENCHMARK:
    run_opt_with_problem (result_dir, problem, opt)

def main():
  (opt, problem) = parsing_arguments()

  # Create a directory for storing evaluation result
  curr_time = datetime.datetime.now().strftime("%H:%M:%S")
  result_dir= "result" + curr_time
  make_dir(result_dir)

  # Default : run all programs with FixML, CAFE, and SARFGEN
  if opt == "all" and problem == None:
    run_opt_all (result_dir, "cafe")
    run_opt_all (result_dir, "fixml")
    run_opt_all (result_dir, "prog")
    run_opt_all (result_dir, "func")

  # Run all submissions of one problem by all options
  if opt == "all" and not problem == None:
    run_opt_with_problem (result_dir, problem, "cafe")
    run_opt_with_problem (result_dir, problem, "fixml")
    run_opt_with_problem (result_dir, problem, "prog")
    run_opt_with_problem (result_dir, problem, "func")

  # Run all submissions by specific option 
  if not opt == "all" and problem == None:
    run_opt_all (result_dir, opt)
  
  # Run all submissions of one problem by specific option 
  if not opt == "all" and not problem == None:
    run_opt_with_problem (result_dir, problem, opt)

if __name__ == '__main__':
  main()