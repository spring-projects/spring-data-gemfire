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
import static org.mockito.Matchers.isA;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.data.gemfire.CacheFactoryBean;

import com.gemstone.gemfire.cache.DiskStore;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;
import com.gemstone.gemfire.internal.cache.PartitionedRegion;

/**
 * The PdxDiskStoreAwareBeanFactoryPostProcessorTest class is a test suite of test cases testing the functionality
 * of the PdxDiskStoreAwareBeanFactoryPostProcessor class.
 * <p/>
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.PdxDiskStoreAwareBeanFactoryPostProcessor
 * @see com.gemstone.gemfire.cache.DiskStore
 * @see com.gemstone.gemfire.cache.Region
 * @see com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue
 * @since 1.3.3
 */
public class PdxDiskStoreAwareBeanFactoryPostProcessorTest {

	protected static String[] toStringArray(final Collection<String> collection) {
		return collection.toArray(new String[collection.size()]);
	}

	protected static boolean isEmpty(final Object[] array) {
		return (array == null || array.length == 0);
	}

	protected static boolean isBeanType(final BeanDefinition beanDefinition, final Class beanType) {
		return (beanDefinition instanceof AbstractBeanDefinition
			&& ((AbstractBeanDefinition) beanDefinition).hasBeanClass()
				&& beanType.isAssignableFrom(((AbstractBeanDefinition) beanDefinition).getBeanClass()));
	}

	protected ConfigurableListableBeanFactory createMockBeanFactory(final Map<String, BeanDefinition> beanDefinitions) {
		final ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class);

		when(mockBeanFactory.getBeanDefinitionNames()).thenReturn(toStringArray(beanDefinitions.keySet()));

		when(mockBeanFactory.getBeanNamesForType(isA(Class.class))).then(new Answer<String[]>() {
			@Override
			public String[] answer(final InvocationOnMock invocation) throws Throwable {
				Object[] arguments = invocation.getArguments();

				assertNotNull(arguments);
				assertTrue(arguments.length == 1);
				assertTrue(arguments[0] instanceof Class);

				Class beanType = (Class) arguments[0];

				List<String> beanNames = new ArrayList<String>(beanDefinitions.size());

				for (Map.Entry<String, BeanDefinition> entry : beanDefinitions.entrySet()) {
					BeanDefinition beanDefinition = entry.getValue();

					if (isBeanType(beanDefinition, beanType)) {
						beanNames.add(entry.getKey());
					}
				}

				return toStringArray(beanNames);
			}
		});

		when(mockBeanFactory.getBeanDefinition(anyString())).then(new Answer<BeanDefinition>() {
			@Override
			public BeanDefinition answer(final InvocationOnMock invocation) throws Throwable {
				Object[] arguments = invocation.getArguments();
				assertNotNull(arguments);
				assertTrue(arguments.length == 1);
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

	protected BeanDefinition defineAsyncEventQueue(String... dependencies) {
		return createBeanDefinitionBuilder(AsyncEventQueue.class, dependencies).getBeanDefinition();
	}

	protected BeanDefinition defineDiskStore(String... dependencies) {
		return createBeanDefinitionBuilder(DiskStore.class, dependencies).getBeanDefinition();
	}

	protected BeanDefinition defineRegion(Class regionClass, String... dependencies) {
		return createBeanDefinitionBuilder(regionClass, dependencies).getBeanDefinition();
	}

	protected BeanDefinition definePartitionedRegion(String... dependencies) {
		return defineRegion(PartitionedRegion.class, dependencies);
	}

	protected BeanDefinition defineReplicatedRegion(String... dependencies) {
		return defineRegion(Region.class, dependencies);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testCreatePdxDiskStoreAwareBeanFactoryPostProcessorWithBlankDiskStoreName() {
		new PdxDiskStoreAwareBeanFactoryPostProcessor("  ");
	}

	@Test(expected = IllegalArgumentException.class)
	public void testCreatePdxDiskStoreAwareBeanFactoryPostProcessorWithEmptyDiskStoreName() {
		new PdxDiskStoreAwareBeanFactoryPostProcessor("");
	}

	@Test(expected = IllegalArgumentException.class)
	public void testCreatePdxDiskStoreAwareBeanFactoryPostProcessorWithNullDiskStoreName() {
		new PdxDiskStoreAwareBeanFactoryPostProcessor(null);
	}

	@Test
	public void testInitializedPdxDiskStoreAwareBeanFactoryPostProcessor() {
		PdxDiskStoreAwareBeanFactoryPostProcessor postProcessor =
			new PdxDiskStoreAwareBeanFactoryPostProcessor("testPdxDiskStoreName");

		assertNotNull(postProcessor);
		assertEquals("testPdxDiskStoreName", postProcessor.getPdxDiskStoreName());
	}

	@Test
	public void testPostProcessBeanFactory() {
		final Map<String, BeanDefinition> beanDefinitions = new HashMap<String, BeanDefinition>(13);

		beanDefinitions.put("someBean", defineBean("org.company.app.domain.SomeBean", "someOtherBean"));
		beanDefinitions.put("gemfireCache", defineCache());
		beanDefinitions.put("pdxDiskStore", defineDiskStore());
		beanDefinitions.put("someOtherBean", defineBean("org.company.app.domain.SomeOtherBean"));
		beanDefinitions.put("queue1", defineAsyncEventQueue("someOtherBean"));
		beanDefinitions.put("overflowDiskStore", defineDiskStore());
		beanDefinitions.put("region1", defineReplicatedRegion("overflowDiskStore"));
		beanDefinitions.put("region2DiskStore", defineDiskStore("someBean"));
		beanDefinitions.put("region2", defineReplicatedRegion("region2DiskStore"));
		beanDefinitions.put("colocatedRegion", definePartitionedRegion("residentRegion", "overflowDiskStore"));
		beanDefinitions.put("residentRegionDiskStore", defineDiskStore("someBean", "yetAnotherBean"));
		beanDefinitions.put("residentRegion", definePartitionedRegion("residentRegionDiskStore"));
		beanDefinitions.put("yetAnotherBean", defineBean("org.company.app.domain.YetAnotherBean", "someBean"));
		beanDefinitions.put("queue2", defineAsyncEventQueue());
		beanDefinitions.put("region3", definePartitionedRegion());
		beanDefinitions.put("region4", definePartitionedRegion("queue2"));

		final ConfigurableListableBeanFactory mockBeanFactory = createMockBeanFactory(beanDefinitions);

		final PdxDiskStoreAwareBeanFactoryPostProcessor postProcessor =
			new PdxDiskStoreAwareBeanFactoryPostProcessor("pdxDiskStore");

		postProcessor.postProcessBeanFactory(mockBeanFactory);

		assertDependencies(beanDefinitions.get("someBean"), "someOtherBean");
		assertTrue(isEmpty(beanDefinitions.get("gemfireCache").getDependsOn()));
		assertTrue(isEmpty(beanDefinitions.get("pdxDiskStore").getDependsOn()));
		assertTrue(isEmpty(beanDefinitions.get("someOtherBean").getDependsOn()));
		assertDependencies(beanDefinitions.get("queue1"), "pdxDiskStore", "someOtherBean");
		assertDependencies(beanDefinitions.get("overflowDiskStore"), "pdxDiskStore");
		assertDependencies(beanDefinitions.get("region1"), "pdxDiskStore", "overflowDiskStore");
		assertDependencies(beanDefinitions.get("region2DiskStore"), "pdxDiskStore", "someBean");
		assertDependencies(beanDefinitions.get("region2"), "pdxDiskStore", "region2DiskStore");
		assertDependencies(beanDefinitions.get("colocatedRegion"), "pdxDiskStore", "residentRegion", "overflowDiskStore");
		assertDependencies(beanDefinitions.get("residentRegionDiskStore"), "pdxDiskStore", "someBean", "yetAnotherBean");
		assertDependencies(beanDefinitions.get("residentRegion"), "pdxDiskStore", "residentRegionDiskStore");
		assertDependencies(beanDefinitions.get("yetAnotherBean"), "someBean");
		assertDependencies(beanDefinitions.get("queue2"), "pdxDiskStore");
		assertDependencies(beanDefinitions.get("region3"), "pdxDiskStore");
		assertDependencies(beanDefinitions.get("region4"), "pdxDiskStore", "queue2");
	}

}
