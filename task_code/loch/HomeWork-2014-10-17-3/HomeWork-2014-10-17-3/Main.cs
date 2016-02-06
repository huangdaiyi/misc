using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HomeWork_2014_10_17_3
{
    class MainClass
    {
        static void Main(string[] args)
        {
            string inputStr, result;
            int ruleID;
            double inputValue;
            Rebate rebate = new Rebate();

            do
            {
                rebate.ShowRules();
                inputStr = Console.ReadLine().Trim();
                if (inputStr == rebate.Exit) break;

                try { ruleID = int.Parse(inputStr); }
                catch (Exception e)
                {
                    Console.WriteLine("Error:" + e.Message);
                    continue;
                }

                if (ruleID > 0 && ruleID <= rebate.Count)
                {
                    rebate.ChooseRule(ruleID);
                    do
                    {
                        rebate.ShowTips();
                        inputStr = Console.ReadLine().Trim();
                        if (inputStr == rebate.Return) break;
                        if (inputStr == rebate.SetRule)
                        {
                            rebate.ShowSetTips();
                            inputStr = Console.ReadLine().Trim();

                            try { inputValue = double.Parse(inputStr); }
                            catch (Exception e)
                            {
                                Console.WriteLine("Error:" + e.Message);
                                continue;
                            }
                            rebate.ChangeRule(inputValue);
                            continue;
                        }

                        try { inputValue = double.Parse(inputStr); }
                        catch (Exception e)
                        {
                            Console.WriteLine("Error:" + e.Message);
                            continue;
                        }
                        result = rebate.GetResult(inputValue);
                        Console.WriteLine(result);
                    } while (true);
                }
            } while (true);
        }
    }
}
