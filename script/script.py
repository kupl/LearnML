from util import *
from matching import *
from run import *
from dd import *
from fix import *
import argparse

#problem directory, entry function, grading program, testcases
benchmark_info = [
  ("KoreaUniv_filter", "filter", "", "testcases/filter_testcases"),
  ("sigma", "sigma", "", ""),
  ("KoreaUniv_sigma", "sigma", "", ""),
]

'''
("max", "max", "", "testcases/max_testcases"),
("KoreaUniv_mirror", "mirror", "", "testcases/iter_testcases"),
("KoreaUniv_mem", "mem", "", "testcases/iter_testcases"),
("KoreaUniv_filter", "filter", "", "testcases/filter_testcases")
("sigma", "sigma", "", ""),
("KoreaUniv_sigma", "sigma", "", ""),

("iter", "iter", "", "testcases/iter_testcases"),
("KoreaUniv_iter", "iter", "", "testcases/iter_testcases"),
("crazy2add". "crazy2add", "", ""),
("formula", "eval", "", "testcases/formula_testcases"),
("KoreaUniv_formula_16", "check", "", "testcases/formula_testcases"),

("All_lambda", "check", "", "testcases/lambda_testcases"),
("diff", "grading", "grading.ml", "testcases/diff_testcases"),
'''
native = "./engine/main.native" 

def main() :
  parser = argparse.ArgumentParser(description="Python script for LearnML")
  parser.add_argument ("--option", required=True, choices=["dd", "run", "fix", "matching"], help="Running options") 
  parser.add_argument ("--path", required=False, default="../benchmarks_incorrect", help="Path of testing benchmarks") 
  
  args = parser.parse_args()
  path = args.path
  option = args.option

  for (problem, entry, grading, testcases) in benchmark_info :
    f = open("./result/" + problem + "_" + option + ".txt", 'w')
    problem_path = os.path.join (path, problem)
    test_path = "../" + testcases

    if grading != "":
      grading = problem_path + "/" + grading
      
    year_list = find_dir(os.path.join(problem_path))
    year_list.sort()
    for year in year_list :
      year_path = os.path.join (problem_path, year)
      submissions = find_files(year_path)
      submissions.sort()
      for submission in submissions:
        submission_path = os.path.join (year_path, submission)
        if (option == "run"):
          out = run_testcases (entry, submission_path, test_path, grading)
        elif (option == "dd"):
          out = run_dd (problem, entry, submission_path, test_path, grading)
        elif (option == "fix"):
          out = run_fix (problem, entry, submission_path, test_path, grading)
        elif (option == "matching"):
          out = find_matchings (problem, submission_path)
        f.write(out)
    f.close()

if __name__ == '__main__': main ()