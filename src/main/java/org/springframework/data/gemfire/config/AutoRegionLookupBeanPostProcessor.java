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

import java.util.Collections;
import java.util.Set;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.Region;

/**
 * The AutoRegionLookupBeanPostProcessor class is a Spring BeanPostProcessor that post processes a GemFireCache by
 * registering all Cache Regions that have not been explicitly defined in the Spring application context.  This is
 * usually the case for Regions that have been defined in GemFire native cache.xml or defined use GemFire 8's new
 * cluster-based configuration service.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.BeanFactoryAware
 * @see org.springframework.beans.factory.config.BeanPostProcessor
 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory
 * @see com.gemstone.gemfire.cache.GemFireCache
 * @see com.gemstone.gemfire.cache.Region
 * @since 1.5.0
 */
public class AutoRegionLookupBeanPostProcessor implements BeanPostProcessor, BeanFactoryAware {

	private ConfigurableListableBeanFactory beanFactory;

	@Override
	public final void setBeanFactory(final BeanFactory beanFactory) throws BeansException {
		Assert.isInstanceOf(ConfigurableListableBeanFactory.class, beanFactory,
			String.format("The BeanFactory reference (%1$s) must be an instance of ConfigurableListableBeanFactory!",
				ObjectUtils.nullSafeClassName(beanFactory)));
		this.beanFactory = (ConfigurableListableBeanFactory) beanFactory;
	}

	protected ConfigurableListableBeanFactory getBeanFactory() {
		Assert.state(this.beanFactory != null, "A reference to the BeanFactory was not properly configured and initialized!");
		return this.beanFactory;
	}

	@Override
	public Object postProcessBeforeInitialization(final Object bean, final String beanName) throws BeansException {
		return bean;
	}

	@Override
	public Object postProcessAfterInitialization(final Object bean, final String beanName) throws BeansException {
		if (bean instanceof GemFireCache) {
			registerCacheRegionsAsBeans((GemFireCache) bean);
		}

		return bean;
	}

	private void registerCacheRegionsAsBeans(final GemFireCache cache) {
		for (Region region : cache.rootRegions()) {
			registerRegionAsBean(region);
		}
	}

	private void registerRegionAsBean(final Region<?, ?> region) {
		if (region != null) {
			String regionBeanName = getBeanName(region);

			if (!getBeanFactory().containsBean(regionBeanName)) {
				getBeanFactory().registerSingleton(regionBeanName, region);
			}

			for (Region<?, ?> subregion : nullSafeSubregions(region)) {
				registerRegionAsBean(subregion);
			}
		}
	}

	private Set<Region<?, ?>> nullSafeSubregions(final Region<?, ?> parentRegion) {
		Set<Region<?, ?>> subregions = parentRegion.subregions(false);
		return (subregions != null ? subregions : Collections.<Region<?, ?>>emptySet());
	}

	private String getBeanName(final Region region) {
		String regionFullPath = region.getFullPath();
		return (regionFullPath.lastIndexOf(Region.SEPARATOR) > 0 ? regionFullPath : region.getName());
	}

}
