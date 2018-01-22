import os
import sys
import subprocess

#test_folders = ["iter","diff","eval","formula","zipper","zipperN"]

#test_folders = ["iter","diff","eval","formula","sigma","zipper","zipperN","crazy2add"]
##################################################
#  Tuple means (folder,entry,grading,testcases)  #
##################################################
#benchmark_info = [("checkMetro","checkMetro","","testcases")]

benchmark_info = [
  ("checkMetro","checkMetro","","testcases"),
  ("crazy2add","grading","grading.ml","testcases"),
  ("crazy2val","crazy2val","","testcases"),
  ("diff","grading","grading.ml","testcases"),
  ("eval","eval","","testcases"),
  ("formula","eval","","testcases"),
  ("iter","iter","","testcases"),
  ("merge","merge","","testcases"),
  ("nat","natadd","","add_testcases"),
  ("nat","natmul","","mul_testcases"),
  ("parenize","toParen","","testcases"),
  ("priority_queue","grading","grading.ml","testcases"),
  ("sigma","sigma","","testcases"),
  ("zipper","zipper","","testcases"),
  ("zipperN","zipperN","","testcases"),
  ("ziptree","goDown","","goDown_testcases"),
  ("ziptree","goRight","","goRight_testcases"),
  ("ziptree","goUp","","goUp_testcases")
  #sumprod, mathemadiga -> float
  #queue, ZExpr -> module
  ]

test_folders=["nat"]

fail_count = 0
succ_count = 0


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

def find_files(path):
  whole_list = os.listdir(path)
  result = []
  for f in whole_list :
    result.append(os.path.join(path,f))
  return result

def gen_command(submission,path,entry,grading,specs):
  opt_testcases = " -testcases "+(os.path.join(path,specs))
  opt_solution = " -solution "+(os.path.join(path,'sol.ml'))
  opt_entryfunction = " -entry "+entry
  opt_submission = " -submission "+submission
  opt_modulespec = " -external engine/moduleSpec.ml"
  if(grading == ""):
    opt_grading = ""
  else:
    opt_grading = " -grading "+(os.path.join(path,grading))
  command = "engine/main.native -run"+opt_submission+opt_modulespec+opt_solution+opt_entryfunction+opt_testcases+opt_grading
  return command

def execute(submission,path,entry,grading,specs):
  command = gen_command(submission,path,entry,grading,specs)
  p = subprocess.Popen(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,shell=True)
  out, err = p.communicate ()
  #global fail_count
  #global succ_count
  #print(command)
  if not err:
    #print(command)
    #if(out.find("None")==-1):
      #succ_count=succ_count+1
    #else:
      #fail_count=fail_count+1
    #print(out)
    return True
  else:
    #print(command)
    #print(err)
    return False

def main() :
  path = os.path.dirname(__file__)
  path = os.path.join(path,'benchmarks')
  commands = []

  for (hw,entry,grading,specs) in benchmark_info :
    count=0
    err_count=0
    year_list = find_dir(os.path.join(path,hw))
    for year in year_list :
      submissions = find_files(year)
      for submission in submissions:
        count=count+1
        b=execute(submission,os.path.join(path,hw),entry,grading,specs)
        if b is False:
          err_count = err_count+1
        else:
          pass
    print(hw)
    print("Submissions: ",count)
    print("Error: ",err_count)

if __name__ == '__main__':
  main()
