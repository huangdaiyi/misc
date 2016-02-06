namespace Goods
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            RebateRuleFactory.Instance.Register("QuantityRebate", new QuantityRebateRule()
            {
                RebateAmt = 5.86m,
                Describion = "按照数量返利，每采购1个商品，供应商会给我们$5.86的返利"
            });
            RebateRuleFactory.Instance.Register("QuantityRegionRebate", new QuantityRegionRebateRule()
            {
                RebateAmt = 8.5m,
                MinQuantity = 100,
                Describion = "按照数量返利，当采购商品的数量大于等于100的时候才能够获取返利，并且每个返利$8.5"
            });
            RebateRuleFactory.Instance.Register("AmtRebate", new AmtRebateRule()
            {
                Percent = 0.105m,
                Describion = "按照金额返利，按照采购金额的10.5%返利"
            });
            RebateRuleFactory.Instance.Register("QuantityRebate", new AmtRegionRebateRule()
            {
                MinAmt = 200,
                Percent = 0.208m,
                Describion = "按照金额返利，当采购金额超过$200的时候才能够有返利，返利按照采购金额的20.8%收取"
            });

            var goods = new Goods()
            {
                Amt = 100,
                Quantity = 100
            };

            var key = "QuantityRebate";
            var result = RebateRuleFactory.Instance.Calculate(key, goods);
        }
    }
}