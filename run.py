import os
import sys
import subprocess

#test_folders = ["iter","diff","eval","formula","zipper","zipperN"]

#test_folders = ["iter","diff","eval","formula","sigma","zipper","zipperN","crazy2add"]
##################################################
#  Tuple means (folder,entry,grading,testcases)  #
##################################################

benchmark_info = [("nat","natmul","","mul_testcases")]
"""
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
"""
test_folders=["nat"]

fail_count = 0
succ_count = 0
try_count = 0
score_result = dict()

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
  
  if(sys.argv[1]=="run"):
    running_option = "engine/main.native -run"
  elif(sys.argv[1]=="fix"):
    running_option = "engine/main.native -fix"
  else:
    print("mode : fix || run")
    sys.exit(0)
  command = running_option+opt_submission+opt_modulespec+opt_solution+opt_entryfunction+opt_testcases+opt_grading
  return command

def execute(submission,path,entry,grading,specs):
  global score_result
  global fail_count
  global succ_count
  global try_count
  
  command = gen_command(submission,path,entry,grading,specs)
  p = subprocess.Popen(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,shell=True)
  out, err = p.communicate ()
  
  #print(command)
  if not err:
    if(sys.argv[1]=="run"):
      score = out.split("score : ")[1]
      if score in score_result:
        score_result[score] = score_result[score]+1
      else:
        score_result[score] = 1

    elif(sys.argv[1]=="fix"):
      if (out.find("The submission is correct code")>=0):
        pass
      elif (out.find("None") == -1):
        try_count=try_count+1
        succ_count=succ_count+1
        print(command)
        print(out)
      else:
        try_count=try_count+1
        fail_count=fail_count+1
        print(command)
        print(out)

    else:
      print("mode : fix || run")
      sys.exit(0)

    return True
   
  else:
    #print(command)
    #print(err)
    return False

def main() :
  path = os.path.dirname(__file__)
  path = os.path.join(path,'benchmarks')
  commands = []
  global score_result
  global fail_count
  global succ_count
  global try_count

  for (hw,entry,grading,specs) in benchmark_info :
    fail_count=0
    succ_count=0
    try_count=0
    count=0
    err_count=0
    score_result=dict()
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
    if(sys.argv[1]=="run"):
      print(hw)
      print("Submissions: ",count)
      print("Error: ",err_count)
      print("Score: ",score_result)
    elif (sys.argv[1]=="fix"):
      print(hw)
      print("Submissions: ",count)
      print("Error: ",err_count)
      print("try count: ",try_count)
      print("success count: ",succ_count)
      print("fail count: ",fail_count)

if __name__ == '__main__':
  if len(sys.argv)!=2:
    print("usage : python run.py [mode]")
  else:
    main()
