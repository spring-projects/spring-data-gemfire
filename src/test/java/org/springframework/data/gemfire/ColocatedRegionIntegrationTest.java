package org.springframework.data.gemfire;

import static org.junit.Assert.*;

import javax.annotation.Resource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Region;

/**
 * The ColocatedRegionIntegrationTest class is a test suite class containing test cases for JIRA issue SGF-195,
 * concerning colocated Regions in GemFire.
 * <p/>
 * @author John Blum
 * @link https://jira.springsource.org/browse/SGF-195
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.3.2
 */
@ContextConfiguration("/org/springframework/data/gemfire/colocated-region.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@SuppressWarnings("unused")
public class ColocatedRegionIntegrationTest {

	@Resource(name = "colocatedRegion")
	private Region colocatedRegion;

	@Resource(name = "sourceRegion")
	private Region sourceRegion;

	protected static void assertRegionExists(final String expectedRegionName, final Region region) {
		assertNotNull(region);
		assertEquals(String.format("Expected Region with name %1$s; but was %2$s!",
			expectedRegionName, region.getName()), expectedRegionName, region.getName());
	}

	@Test
	public void testRegionsColocated() {
		assertRegionExists("Source", sourceRegion);
		assertRegionExists("Colocated", colocatedRegion);
		assertNotNull(colocatedRegion.getAttributes());
		assertNotNull(colocatedRegion.getAttributes().getPartitionAttributes());
		assertEquals(sourceRegion.getName(), colocatedRegion.getAttributes().getPartitionAttributes().getColocatedWith());
	}

}
