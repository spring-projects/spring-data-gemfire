/*
 * Copyright 2010-2018 the original author or authors.
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

package org.springframework.data.gemfire.config.support;

import static org.assertj.core.api.Assertions.assertThat;
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

import org.apache.geode.cache.DiskStore;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.internal.cache.PartitionedRegion;
import org.junit.Test;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.util.ArrayUtils;

/**
 * Unit tests for {@link PdxDiskStoreAwareBeanFactoryPostProcessor}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.support.PdxDiskStoreAwareBeanFactoryPostProcessor
 * @see org.apache.geode.cache.DiskStore
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueue
 * @since 1.3.3
 */
public class PdxDiskStoreAwareBeanFactoryPostProcessorUnitTests {

	protected static boolean isBeanType(BeanDefinition beanDefinition, Class<?> beanType) {
		return (beanDefinition instanceof AbstractBeanDefinition
			&& ((AbstractBeanDefinition) beanDefinition).hasBeanClass()
			&& beanType.isAssignableFrom(((AbstractBeanDefinition) beanDefinition).getBeanClass()));
	}

	protected static String[] toStringArray(Collection<String> collection) {
		return collection.toArray(new String[collection.size()]);
	}

	protected ConfigurableListableBeanFactory mockBeanFactory(final Map<String, BeanDefinition> beanDefinitions) {

		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class);

		when(mockBeanFactory.getBeanDefinitionNames()).thenReturn(toStringArray(beanDefinitions.keySet()));

		when(mockBeanFactory.getBeanNamesForType(isA(Class.class))).then(invocation -> {

			Object[] arguments = invocation.getArguments();

			assertThat(arguments).isNotNull();
			assertThat(arguments.length).isEqualTo(1);
			assertThat(arguments[0]).isInstanceOf(Class.class);

			Class beanType = (Class) arguments[0];

			List<String> beanNames = new ArrayList<>(beanDefinitions.size());

			for (Map.Entry<String, BeanDefinition> entry : beanDefinitions.entrySet()) {
				BeanDefinition beanDefinition = entry.getValue();

				if (isBeanType(beanDefinition, beanType)) {
					beanNames.add(entry.getKey());
				}
			}

			return toStringArray(beanNames);
		});

		when(mockBeanFactory.getBeanDefinition(anyString())).then(invocation -> {

			Object[] arguments = invocation.getArguments();

			assertThat(arguments).isNotNull();
			assertThat(arguments.length).isEqualTo(1);

			return beanDefinitions.get(String.valueOf(arguments[0]));
		});

		return mockBeanFactory;
	}

	protected static BeanDefinitionBuilder newBeanDefinitionBuilder(Object beanClassObject, String... dependencies) {

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
		assertThat(ArrayUtils.isEmpty(beanDefinition.getDependsOn())).isFalse();
		assertThat(Arrays.asList(beanDefinition.getDependsOn()).equals(Arrays.asList(expectedDependencies))).isTrue();
	}

	protected BeanDefinition defineBean(String beanClassName, String... dependencies) {
		return newBeanDefinitionBuilder(beanClassName, dependencies).getBeanDefinition();
	}

	protected BeanDefinition defineCache() {
		return newBeanDefinitionBuilder(CacheFactoryBean.class).getBeanDefinition();
	}

	protected BeanDefinition defineAsyncEventQueue(String... dependencies) {
		return newBeanDefinitionBuilder(AsyncEventQueue.class, dependencies).getBeanDefinition();
	}

	protected BeanDefinition defineDiskStore(String... dependencies) {
		return newBeanDefinitionBuilder(DiskStore.class, dependencies).getBeanDefinition();
	}

	protected BeanDefinition defineRegion(Class regionClass, String... dependencies) {
		return newBeanDefinitionBuilder(regionClass, dependencies).getBeanDefinition();
	}

	protected BeanDefinition definePartitionedRegion(String... dependencies) {
		return defineRegion(PartitionedRegion.class, dependencies);
	}

	protected BeanDefinition defineReplicatedRegion(String... dependencies) {
		return defineRegion(Region.class, dependencies);
	}

	@Test(expected = IllegalArgumentException.class)
	public void createPdxDiskStoreAwareBeanFactoryPostProcessorWithBlankDiskStoreName() {
		new PdxDiskStoreAwareBeanFactoryPostProcessor("  ");
	}

	@Test(expected = IllegalArgumentException.class)
	public void createPdxDiskStoreAwareBeanFactoryPostProcessorWithEmptyDiskStoreName() {
		new PdxDiskStoreAwareBeanFactoryPostProcessor("");
	}

	@Test(expected = IllegalArgumentException.class)
	public void createPdxDiskStoreAwareBeanFactoryPostProcessorWithNullDiskStoreName() {
		new PdxDiskStoreAwareBeanFactoryPostProcessor(null);
	}

	@Test
	public void initializedPdxDiskStoreAwareBeanFactoryPostProcessor() {

		PdxDiskStoreAwareBeanFactoryPostProcessor postProcessor =
			new PdxDiskStoreAwareBeanFactoryPostProcessor("testPdxDiskStoreName");

		assertThat(postProcessor).isNotNull();
		assertThat(postProcessor.getPdxDiskStoreName()).isEqualTo("testPdxDiskStoreName");
	}

	@Test
	@SuppressWarnings("all")
	public void postProcessBeanFactory() {

		Map<String, BeanDefinition> beanDefinitions = new HashMap<String, BeanDefinition>(13);

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

		ConfigurableListableBeanFactory mockBeanFactory = mockBeanFactory(beanDefinitions);

		PdxDiskStoreAwareBeanFactoryPostProcessor postProcessor =
			new PdxDiskStoreAwareBeanFactoryPostProcessor("pdxDiskStore");

		postProcessor.postProcessBeanFactory(mockBeanFactory);

		assertDependencies(beanDefinitions.get("someBean"), "someOtherBean");
		assertThat(ArrayUtils.isEmpty(beanDefinitions.get("gemfireCache").getDependsOn())).isTrue();
		assertThat(ArrayUtils.isEmpty(beanDefinitions.get("pdxDiskStore").getDependsOn())).isTrue();
		assertThat(ArrayUtils.isEmpty(beanDefinitions.get("someOtherBean").getDependsOn())).isTrue();
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
