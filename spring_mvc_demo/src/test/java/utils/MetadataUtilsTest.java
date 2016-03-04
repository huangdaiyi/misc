package utils;

import org.junit.Assert;
import org.junit.Test;

public class MetadataUtilsTest {

	@Test
	public void testFullBackupPathToNormalPath() {
		Assert.assertEquals("userId", MetadataUtils.fullBackupPathToNormalPath("userId/my backup data"));
		Assert.assertEquals("userId/test", MetadataUtils.fullBackupPathToNormalPath("userId/my backup data/test"));
		Assert.assertEquals("userId/test/123", MetadataUtils.fullBackupPathToNormalPath("userId/my backup data/test/123"));
	}

}
