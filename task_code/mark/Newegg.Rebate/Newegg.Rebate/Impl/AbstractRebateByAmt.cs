using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Newegg.Rebate
{
    public abstract class AbstractRebateByAmt : IRebate
    {
        public decimal Percent { get; set; }
        public abstract decimal GetRebateAmt(RebateBaseInfo rebateBaseInfo);
    }
}
