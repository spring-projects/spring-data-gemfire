package org.springframework.data.gemfire;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.gemstone.gemfire.cache.PartitionAttributes;
import com.gemstone.gemfire.cache.PartitionResolver;
import com.gemstone.gemfire.cache.Region;

import org.junit.Test;
import org.springframework.data.gemfire.test.MockRegionFactory;
import org.springframework.data.gemfire.test.StubCache;

/**
 * The PartitionAttributesFactoryBeanTest class is test suite of test cases testing the contract and functionality of
 * the PartitionAttributesFactoryBean class.
 * <p/>
 * @author John Blum
 * @see org.springframework.data.gemfire.PartitionAttributesFactoryBean
 * @see org.junit.Test
 * @since 1.3.2
 */
@SuppressWarnings("unused")
public class PartitionAttributesFactoryBeanTest {

	private final MockRegionFactory regionFactory = new MockRegionFactory(new StubCache());

	protected Region createMockRegion(final String name) {
		return regionFactory.mockRegion(name);
	}

	protected PartitionResolver createMockPartitionResolver(final String name) {
		PartitionResolver partitionResolver = mock(PartitionResolver.class);

		when(partitionResolver.getName()).thenReturn(name);

		return partitionResolver;
	}

	@Test
	public void testSetBasicProperties() throws Exception {
		PartitionAttributesFactoryBean partitionAttributesFactoryBean = new PartitionAttributesFactoryBean();

		partitionAttributesFactoryBean.setColocatedWith(createMockRegion("mockColocatedRegion"));
		partitionAttributesFactoryBean.setLocalMaxMemory(1024);
		partitionAttributesFactoryBean.setPartitionResolver(createMockPartitionResolver("mockPartitionResolver"));
		partitionAttributesFactoryBean.setRecoveryDelay(1000l);
		partitionAttributesFactoryBean.setRedundantCopies(1);
		partitionAttributesFactoryBean.setStartupRecoveryDelay(60000l);
		partitionAttributesFactoryBean.setTotalMaxMemory(8192l);
		partitionAttributesFactoryBean.setTotalNumBuckets(42);

		PartitionAttributes partitionAttributes = partitionAttributesFactoryBean.getObject();

		assertNotNull(partitionAttributes);
		assertEquals("mockColocatedRegion", partitionAttributes.getColocatedWith());
		assertEquals(1024, partitionAttributes.getLocalMaxMemory());
		assertNotNull(partitionAttributes.getPartitionResolver());
		assertEquals("mockPartitionResolver", partitionAttributes.getPartitionResolver().getName());
		assertEquals(1000l, partitionAttributes.getRecoveryDelay());
		assertEquals(1, partitionAttributes.getRedundantCopies());
		assertEquals(60000l, partitionAttributes.getStartupRecoveryDelay());
		assertEquals(8192l, partitionAttributes.getTotalMaxMemory());
		assertEquals(42, partitionAttributes.getTotalNumBuckets());
	}

}
