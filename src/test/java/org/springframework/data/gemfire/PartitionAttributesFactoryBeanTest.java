/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.apache.geode.cache.PartitionAttributes;
import org.apache.geode.cache.PartitionResolver;

import org.junit.Test;

/**
 * Unit tests for {@link PartitionAttributesFactoryBean}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.PartitionAttributesFactoryBean
 * @since 1.3.3
 */
@SuppressWarnings("unused")
public class PartitionAttributesFactoryBeanTest {

	protected PartitionResolver createMockPartitionResolver(final String name) {
		PartitionResolver partitionResolver = mock(PartitionResolver.class);

		when(partitionResolver.getName()).thenReturn(name);

		return partitionResolver;
	}

	@Test
	public void testSetBasicProperties() throws Exception {
		PartitionAttributesFactoryBean partitionAttributesFactoryBean = new PartitionAttributesFactoryBean();

		partitionAttributesFactoryBean.setColocatedWith("mockColocatedRegion");
		partitionAttributesFactoryBean.setLocalMaxMemory(1024);
		partitionAttributesFactoryBean.setPartitionResolver(createMockPartitionResolver("mockPartitionResolver"));
		partitionAttributesFactoryBean.setRecoveryDelay(1000L);
		partitionAttributesFactoryBean.setRedundantCopies(1);
		partitionAttributesFactoryBean.setStartupRecoveryDelay(60000L);
		partitionAttributesFactoryBean.setTotalMaxMemory(8192L);
		partitionAttributesFactoryBean.setTotalNumBuckets(42);
		partitionAttributesFactoryBean.afterPropertiesSet();

		PartitionAttributes partitionAttributes = partitionAttributesFactoryBean.getObject();

		assertNotNull(partitionAttributes);
		assertEquals("mockColocatedRegion", partitionAttributes.getColocatedWith());
		assertEquals(1024, partitionAttributes.getLocalMaxMemory());
		assertNotNull(partitionAttributes.getPartitionResolver());
		assertEquals("mockPartitionResolver", partitionAttributes.getPartitionResolver().getName());
		assertEquals(1000L, partitionAttributes.getRecoveryDelay());
		assertEquals(1, partitionAttributes.getRedundantCopies());
		assertEquals(60000L, partitionAttributes.getStartupRecoveryDelay());
		assertEquals(8192L, partitionAttributes.getTotalMaxMemory());
		assertEquals(42, partitionAttributes.getTotalNumBuckets());
	}

	@Test(expected = IllegalStateException.class)
	public void testValidationOnRedundantCopiesWhenExceedsBound() throws Exception {
		PartitionAttributesFactoryBean partitionAttributesFactoryBean = new PartitionAttributesFactoryBean();
		partitionAttributesFactoryBean.setRedundantCopies(4);
		partitionAttributesFactoryBean.afterPropertiesSet();
	}

	@Test(expected = IllegalStateException.class)
	public void testValidationOnRedundantCopiesWhenPrecedesBound() throws Exception {
		PartitionAttributesFactoryBean partitionAttributesFactoryBean = new PartitionAttributesFactoryBean();
		partitionAttributesFactoryBean.setRedundantCopies(-1);
		partitionAttributesFactoryBean.afterPropertiesSet();
	}
}
