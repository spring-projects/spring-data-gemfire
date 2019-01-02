/*
 * Copyright 2018-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeCollection;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.StreamSupport;

import org.apache.geode.cache.Region;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.RegionConfigurer;

/**
 * {@link ConfigurableRegionFactoryBean} is an abstract base class encapsulating functionality common
 * to all configurable {@link Region} {@link FactoryBean FactoryBeans}.
 *
 * A {@literal configurable} {@link Region} {@link FactoryBean} includes all {@link FactoryBean FactoryBeans}
 * that create a {@link Region} and allow additional configuration to be applied via a {@link RegionConfigurer}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.data.gemfire.ResolvableRegionFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
 * @since 2.1.0
 */
@SuppressWarnings("unused")
public abstract class ConfigurableRegionFactoryBean<K, V> extends ResolvableRegionFactoryBean<K, V> {

	private List<RegionConfigurer> regionConfigurers = Collections.emptyList();

	private RegionConfigurer compositeRegionConfigurer = new RegionConfigurer() {

		@Override
		public void configure(String beanName, ClientRegionFactoryBean<?, ?> bean) {
			nullSafeCollection(regionConfigurers)
				.forEach(regionConfigurer -> regionConfigurer.configure(beanName, bean));
		}
	};

	/**
	 * Returns a reference to the Composite {@link RegionConfigurer} used to apply additional configuration
	 * to this {@link ClientRegionFactoryBean} on Spring container initialization.
	 *
	 * @return the Composite {@link RegionConfigurer}.
	 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
	 */
	protected RegionConfigurer getCompositeRegionConfigurer() {
		return this.compositeRegionConfigurer;
	}

	/**
	 * Null-safe operation to set an array of {@link RegionConfigurer RegionConfigurers} used to apply
	 * additional configuration to this {@link ClientRegionFactoryBean} when using Annotation-based configuration.
	 *
	 * @param regionConfigurers array of {@link RegionConfigurer RegionConfigurers} used to apply
	 * additional configuration to this {@link ClientRegionFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
	 * @see #setRegionConfigurers(List)
	 */
	public void setRegionConfigurers(RegionConfigurer... regionConfigurers) {
		setRegionConfigurers(Arrays.asList(nullSafeArray(regionConfigurers, RegionConfigurer.class)));
	}

	/**
	 * Null-safe operation to set an {@link Iterable} of {@link RegionConfigurer RegionConfigurers} used to apply
	 * additional configuration to this {@link ClientRegionFactoryBean} when using Annotation-based configuration.
	 *
	 * @param regionConfigurers {@link Iterable} of {@link RegionConfigurer RegionConfigurers} used to apply
	 * additional configuration to this {@link ClientRegionFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
	 */
	public void setRegionConfigurers(List<RegionConfigurer> regionConfigurers) {
		this.regionConfigurers = Optional.ofNullable(regionConfigurers).orElseGet(Collections::emptyList);
	}

	/**
	 * Null-safe operation to apply the composite {@link RegionConfigurer RegionConfigurers}
	 * to this {@link ConfigurableRegionFactoryBean}.
	 *
	 * @param regionName {@link String} containing the name of the {@link Region}.
	 * to this {@link ConfigurableRegionFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
	 * @see #applyRegionConfigurers(String, Iterable)
	 * @see #getCompositeRegionConfigurer()
	 */
	protected void applyRegionConfigurers(String regionName) {
		applyRegionConfigurers(regionName, getCompositeRegionConfigurer());
	}

	/**
	 * Null-safe operation to apply the given array of {@link RegionConfigurer RegionConfigurers}
	 * to this {@link ConfigurableRegionFactoryBean}.
	 *
	 * @param regionName {@link String} containing the name of the {@link Region}.
	 * @param regionConfigurers array of {@link RegionConfigurer RegionConfigurers} applied
	 * to this {@link ConfigurableRegionFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
	 * @see #applyRegionConfigurers(String, Iterable)
	 */
	protected void applyRegionConfigurers(String regionName, RegionConfigurer... regionConfigurers) {
		applyRegionConfigurers(regionName, Arrays.asList(nullSafeArray(regionConfigurers, RegionConfigurer.class)));
	}

	/**
	 * Null-safe operation to apply the given {@link Iterable} of {@link RegionConfigurer RegionConfigurers}
	 * to this {@link ConfigurableRegionFactoryBean}.
	 *
	 * @param regionName {@link String} containing the name of the {@link Region}.
	 * @param regionConfigurers {@link Iterable} of {@link RegionConfigurer RegionConfigurers} applied
	 * to this {@link ConfigurableRegionFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.RegionConfigurer
	 * @see #applyRegionConfigurers(String, RegionConfigurer...)
	 */
	protected void applyRegionConfigurers(String regionName, Iterable<RegionConfigurer> regionConfigurers) {

		if (this instanceof PeerRegionFactoryBean) {
			StreamSupport.stream(nullSafeIterable(regionConfigurers).spliterator(), false)
				.forEach(regionConfigurer -> regionConfigurer.configure(regionName, (PeerRegionFactoryBean<K, V>) this));
		}
		else if (this instanceof ClientRegionFactoryBean) {
			StreamSupport.stream(nullSafeIterable(regionConfigurers).spliterator(), false)
				.forEach(regionConfigurer -> regionConfigurer.configure(regionName, (ClientRegionFactoryBean<K, V>) this));
		}
	}
}
