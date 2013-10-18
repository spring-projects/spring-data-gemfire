/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.DiskStoreFactoryBean;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.ReplicatedRegionFactoryBean;

/**
 * The PdxDiskStoreAwareBeanFactoryPostProcessorTest class is a test suite of test cases testing the functionality
 * of the PdxDiskStoreAwareBeanFactoryPostProcessor class.
 * <p/>
 * @author John Blum
 * @see org.mockito.Mockito
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.config.PdxDiskStoreAwareBeanFactoryPostProcessor
 * @since 1.3.3
 */
public class PdxDiskStoreAwareBeanFactoryPostProcessorTest {

	protected static final boolean PERSISTENT = true;
	protected static final boolean NOT_PERSISTENT = false;

	protected static String[] toStringArray(final Collection<String> collection) {
		return collection.toArray(new String[collection.size()]);
	}

	protected static boolean isEmpty(final Object[] array) {
		return (array == null || array.length == 0);
	}

	protected ConfigurableListableBeanFactory createMockBeanFactory(final Map<String, BeanDefinition> beanDefinitions) {
		final ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class);

		when(mockBeanFactory.getBeanDefinitionNames()).thenReturn(toStringArray(beanDefinitions.keySet()));

		when(mockBeanFactory.getBeanDefinition(anyString())).then(new Answer<BeanDefinition>() {
			@Override
			public BeanDefinition answer(final InvocationOnMock invocation) throws Throwable {
				final Object[] arguments = invocation.getArguments();
				assertNotNull(arguments);
				assertTrue(arguments.length > 0);
				return beanDefinitions.get(String.valueOf(arguments[0]));
			}
		});

		return mockBeanFactory;
	}

	protected static BeanDefinitionBuilder createBeanDefinitionBuilder(Object beanClassObject, String... dependencies) {
		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition();

		if (beanClassObject instanceof Class) {
			builder.getRawBeanDefinition().setBeanClass((Class) beanClassObject);
		}
		else {
			builder.getRawBeanDefinition().setBeanClassName(String.valueOf(beanClassObject));
		}

		return addDependsOn(builder, dependencies);
	}

	protected static BeanDefinitionBuilder addDependsOn(BeanDefinitionBuilder builder, String... dependencies) {
		for (String dependency : dependencies) {
			builder.addDependsOn(dependency);
		}

		return builder;
	}

	protected static void assertDependencies(BeanDefinition beanDefinition, String... expectedDependencies) {
		assertFalse(isEmpty(beanDefinition.getDependsOn()));
		assertTrue(Arrays.asList(beanDefinition.getDependsOn()).equals(Arrays.asList(expectedDependencies)));
	}

	protected BeanDefinition defineBean(String beanClassName, String... dependencies) {
		return createBeanDefinitionBuilder(beanClassName, dependencies).getBeanDefinition();
	}

	protected BeanDefinition defineCache() {
		return createBeanDefinitionBuilder(CacheFactoryBean.class).getBeanDefinition();
	}

	protected BeanDefinition defineDiskStore(String... dependencies) {
		return createBeanDefinitionBuilder(DiskStoreFactoryBean.class, dependencies).getBeanDefinition();
	}

	protected BeanDefinition defineRegion(Class beanClass, boolean persistent, String... dependencies) {
		BeanDefinitionBuilder builder = createBeanDefinitionBuilder(beanClass, dependencies);
		builder.addPropertyValue("persistent", persistent);
		return builder.getBeanDefinition();
	}

	protected BeanDefinition definePartitionedRegion(boolean persistent, String... dependencies) {
		return defineRegion(PartitionedRegionFactoryBean.class, persistent, dependencies);
	}

	protected BeanDefinition defineReplicatedRegion(boolean persistent, String... dependencies) {
		return defineRegion(ReplicatedRegionFactoryBean.class, persistent, dependencies);
	}

	@Test
	public void testInitializedPdxDiskStoreAwareBeanFactoryPostProcessor() {
		final PdxDiskStoreAwareBeanFactoryPostProcessor postProcessor =
			new PdxDiskStoreAwareBeanFactoryPostProcessor("testPdxDiskStoreName");

		assertNotNull(postProcessor);
		assertEquals("testPdxDiskStoreName", postProcessor.getPdxDiskStoreName());

		postProcessor.setPdxDiskStoreName("mockPdxDiskStoreName");

		assertEquals("mockPdxDiskStoreName", postProcessor.getPdxDiskStoreName());
	}

	@Test(expected = IllegalStateException.class)
	public void testUninitializedPdxDiskStoreAwareBeanFactoryPostProcessor() {
		new PdxDiskStoreAwareBeanFactoryPostProcessor().getPdxDiskStoreName();
	}

	@Test
	public void testPostProcessBeanFactory() {
		final Map<String, BeanDefinition> beanDefinitions = new HashMap<String, BeanDefinition>(13);

		beanDefinitions.put("someBean", defineBean("org.company.app.domain.SomeBean", "someOtherBean"));
		beanDefinitions.put("gemfireCache", defineCache());
		beanDefinitions.put("pdxDiskStore", defineDiskStore());
		beanDefinitions.put("someOtherBean", defineBean("org.company.app.domain.SomeOtherBean"));
		beanDefinitions.put("overflowDiskStore", defineDiskStore());
		beanDefinitions.put("region1", defineReplicatedRegion(NOT_PERSISTENT, "overflowDiskStore"));
		beanDefinitions.put("region2DiskStore", defineDiskStore("someBean"));
		beanDefinitions.put("region2", defineReplicatedRegion(PERSISTENT, "region2DiskStore"));
		beanDefinitions.put("colocatedRegion", definePartitionedRegion(NOT_PERSISTENT, "residentRegion",
			"overflowDiskStore"));
		beanDefinitions.put("residentRegionDiskStore", defineDiskStore("someBean", "yetAnotherBean"));
		beanDefinitions.put("residentRegion", definePartitionedRegion(PERSISTENT, "residentRegionDiskStore"));
		beanDefinitions.put("yetAnotherBean", defineBean("org.company.app.domain.YetAnotherBean", "someBean"));

		final ConfigurableListableBeanFactory mockBeanFactory = createMockBeanFactory(beanDefinitions);

		final PdxDiskStoreAwareBeanFactoryPostProcessor postProcessor =
			new PdxDiskStoreAwareBeanFactoryPostProcessor("pdxDiskStore");

		postProcessor.postProcessBeanFactory(mockBeanFactory);

		assertDependencies(beanDefinitions.get("someBean"), "someOtherBean");
		assertTrue(isEmpty(beanDefinitions.get("gemfireCache").getDependsOn()));
		assertTrue(isEmpty(beanDefinitions.get("pdxDiskStore").getDependsOn()));
		assertTrue(isEmpty(beanDefinitions.get("someOtherBean").getDependsOn()));
		assertDependencies(beanDefinitions.get("overflowDiskStore"), "pdxDiskStore");
		assertDependencies(beanDefinitions.get("region1"), "overflowDiskStore");
		assertDependencies(beanDefinitions.get("region2DiskStore"), "pdxDiskStore", "someBean");
		assertDependencies(beanDefinitions.get("region2"), "pdxDiskStore", "region2DiskStore");
		assertDependencies(beanDefinitions.get("colocatedRegion"), "residentRegion", "overflowDiskStore");
		assertDependencies(beanDefinitions.get("residentRegionDiskStore"), "pdxDiskStore", "someBean", "yetAnotherBean");
		assertDependencies(beanDefinitions.get("residentRegion"), "pdxDiskStore", "residentRegionDiskStore");
		assertDependencies(beanDefinitions.get("yetAnotherBean"), "someBean");
	}

}
