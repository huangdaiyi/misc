using System.Collections.Generic;

namespace Goods
{
    public abstract class RebateRule
    {
        public abstract decimal Calculate(Goods goods);

        public string Describion { get; set; }
    }

    public class QuantityRebateRule : RebateRule
    {
        public decimal RebateAmt { get; set; }

        public virtual decimal Calculate(Goods goods)
        {
            return goods.Quantity * RebateAmt;
        }
    }

    public class QuantityRegionRebateRule : QuantityRebateRule
    {
        public int MinQuantity { get; set; }

        public override decimal Calculate(Goods goods)
        {
            return goods.Quantity >= MinQuantity
                ? base.Calculate(goods)
                : 0m;
        }
    }

    public class AmtRebateRule : RebateRule
    {
        public decimal Percent { get; set; }

        public virtual decimal Calculate(Goods goods)
        {
            return goods.Amt * Percent;
        }
    }

    public class AmtRegionRebateRule : AmtRebateRule
    {
        public decimal MinAmt { get; set; }

        public decimal Calculate(Goods goods)
        {
            return goods.Amt > MinAmt
                 ? base.Calculate(goods)
                 : 0m;
        }
    }

    public sealed class RebateRuleFactory
    {
        public static readonly RebateRuleFactory Instance = new RebateRuleFactory();

        public Dictionary<string, RebateRule> Rules = new Dictionary<string, RebateRule>();

        public void Register(string key, RebateRule rule)
        {
            if (!Rules.ContainsKey(key))
            {
                Rules.Add(key, rule);
            }
        }

        public decimal Calculate(string Key, Goods goods)
        {
            RebateRule rule = null;
            Rules.TryGetValue(Key, out rule);
            return rule != null ? rule.Calculate(goods) : 0m;
        }
    }
}