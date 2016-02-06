using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HomeWork_2014_10_17_3
{
    abstract class Rule
    {
        protected string describe;
        protected string tips;
        protected string setTips;
        protected double result;

        public string Describe { get { return describe; } }
        public string Tips { get { return tips; } }
        public string SetTips { get { return setTips; } }

        public Rule()
        {
            Init();
        }
        protected abstract void Init();
        public abstract void ChangeRule(double newValue);
        public abstract double GetResult(double inputValue);
    }

    class Rule1 : Rule
    {
        private double eachGoodsRebate = 5.86;

        protected override void Init()
        {
            describe = "每个商品返利$" + eachGoodsRebate;
            tips = describe+"，输入商品数量计算返利金额";
            setTips = describe + "，输入新值进行修改：";
        }

        public override void ChangeRule(double newValue)
        {
            eachGoodsRebate = newValue;
            Init();
        }

        public override double GetResult(double inputeValue)
        {
            result = eachGoodsRebate * inputeValue;
            return result;
        }
    }

    class Rule2 : Rule
    {
        private double eachGoodsRebate = 8.5;
        private int goodsCountMin = 100;

        protected override void Init()
        {
            describe = "当商品数量>="+goodsCountMin+"时每个商品返利$" + eachGoodsRebate;
            tips = describe + "，输入商品数量计算返利金额";
            setTips = "每个商品返利$" + eachGoodsRebate + "，输入新值进行修改：";
        }

        public override void ChangeRule(double newValue)
        {
            eachGoodsRebate = newValue;
            Init();
        }

        public override double GetResult(double inputeValue)
        {
            if (inputeValue >= goodsCountMin)
                result = eachGoodsRebate * inputeValue;
            else
                result = 0;
            return result;
        }
    }

    class Rule3 : Rule
    {
        private double rebateRate = 0.105;

        protected override void Init()
        {
            describe = "返利采购金额的" + rebateRate * 100 + "%";
            tips = describe + "，输入采购金额计算返利金额";
            setTips = describe + "，输入新值（小数）进行修改：";
        }

        public override void ChangeRule(double newValue)
        {
            rebateRate = newValue;
            Init();
        }

        public override double GetResult(double inputeValue)
        {
            result = rebateRate * inputeValue;
            return result;
        }
    }

    class Rule4 : Rule
    {
        private double rebateRate = 0.105;
        private double purchaseAmountMin = 200;

        protected override void Init()
        {
            describe = "采购金额>=$" + purchaseAmountMin + "时返利" + rebateRate * 100 + "%";
            tips = describe + "，输入采购金额计算返利金额";
            setTips = "返利采购金额的" + rebateRate * 100 + "%" + "，输入新值（小数）进行修改：";
        }

        public override void ChangeRule(double newValue)
        {
            rebateRate = newValue;
            Init();
        }

        public override double GetResult(double inputeValue)
        {
            if (inputeValue >= purchaseAmountMin)
                result = rebateRate * inputeValue;
            else
                result = 0;
            return result;
        }
    }

    class Rebate
    {
        private int ruleID;
        private double result;
        private List<Rule> rules = new List<Rule>();

        public int Count { get { return rules.Count; } }
        public string Exit { get { return "0"; } }
        public string Return { get { return "#"; } }
        public string SetRule { get { return "*"; } }

        public Rebate()
        {
            rules.Add(new Rule1());
            rules.Add(new Rule2());
            rules.Add(new Rule3());
            rules.Add(new Rule4());
        }

        public void ShowRules()
        {
            Console.WriteLine("Enter a number below to choose a rule, or enter '{0}' to exit.", Exit);
            for (int i = 0; i < Count; i++)
            {
                Console.WriteLine((i + 1) + " " + rules[i].Describe);
            }
            Console.WriteLine(Exit + " Exit!");
        }

        public void ChooseRule(int ruleID)
        {
            this.ruleID = ruleID - 1;
        }

        public void ShowTips()
        {
            Console.WriteLine("{0}，输入'{1}'修改规则，输入'{2}'重选规则", rules[ruleID].Tips, SetRule, Return);
        }

        public void ShowSetTips()
        {
            Console.WriteLine(rules[ruleID].SetTips);
        }

        public void ChangeRule(double newValue)
        {
            rules[ruleID].ChangeRule(newValue);
        }

        public string GetResult(double inputValue)
        {
            result = rules[ruleID].GetResult(inputValue);
            return "返利总金额：" + result;
        }
    }
}
