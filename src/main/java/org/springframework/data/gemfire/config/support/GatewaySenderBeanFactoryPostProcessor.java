/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.config.support;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.wan.GatewaySender;

import org.springframework.beans.BeansException;
import org.springframework.beans.PropertyValue;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.gemfire.PeerRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.data.gemfire.wan.GatewaySenderFactoryBean;

/**
 * A {@link BeanFactoryPostProcessor} to associate the configured {@link org.apache.geode.cache.wan.GatewaySender}
 * onto the corresponding {@link org.apache.geode.cache.Region}.
 *
 * @author Udo Kohlmeyer
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.wan.GatewaySender
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor
 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory
 * @see org.springframework.beans.factory.config.RuntimeBeanReference
 * @see org.springframework.beans.factory.support.ManagedList
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.PeerRegionFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport
 * @see org.springframework.data.gemfire.wan.GatewaySenderFactoryBean
 * @since 2.2.0
 */
@Configuration
public class GatewaySenderBeanFactoryPostProcessor extends AbstractAnnotationConfigSupport {

	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return null;
	}

	/**
	 * {@link BeanFactoryPostProcessor} assigning {@link GatewaySender GatewaySenders} to {@link Region Regions}.
	 *
	 * @return {@link BeanFactoryPostProcessor} for the {@link GatewaySenderFactoryBean}.
	 * @throws BeansException {@link org.springframework.beans.factory.BeanFactory} post processing fails.
	 * @see #populateBeanDefinitionCache(ConfigurableListableBeanFactory)
	 * @see #groupGatewaySenderPerRegion(Map, Map)
	 * @see #addGatewaySendersToRegionFactory(ConfigurableListableBeanFactory, Map)
	 */
	@Bean
	public BeanFactoryPostProcessor postProcessBeanFactory() throws BeansException {

		return beanFactory -> {

			// Create a map of cached BeanDefinitions. Mapped under 'regions' and 'gatewaySenders' for easier lookups
			Map<String, Map<String, BeanDefinition>> cachedBeanDefinitions = populateBeanDefinitionCache(beanFactory);

			// Create a list of gatewaySender to Regions mapping
			Map<String, List<String>> gatewaySenderToRegions =
				groupGatewaySenderPerRegion(cachedBeanDefinitions, new HashMap<>());

			// Add
			addGatewaySendersToRegionFactory(beanFactory, gatewaySenderToRegions);
		};
	}

	/**
	 * Caches BeanDefinitions for GatewaySenders and Regions. Maps the beandefinitions under the keys of
	 * `regions` and `gatewaySenders`
	 *
	 * @return {@link Map} containing all {@link BeanDefinition BeanDefinitions} for {@link Region Regions}
	 * and {@link GatewaySender GatewaySenders}.
	 */
	private Map<String, Map<String, BeanDefinition>> populateBeanDefinitionCache(
			ConfigurableListableBeanFactory beanFactory) {

		Map<String, Map<String, BeanDefinition>> cachedBeanDefinitions = new LinkedHashMap<>();

		Arrays.stream(ArrayUtils.nullSafeArray(beanFactory.getBeanDefinitionNames(), String.class))
			.forEach(beanName -> {

				BeanDefinition beanDefinition = beanFactory.getBeanDefinition(beanName);

				if (isRegionBean(beanDefinition, beanFactory)) {
					addBeanDefinitionToList(beanName, beanDefinition, cachedBeanDefinitions, "regions");
				}
				else if (isGatewaySenderFactoryBean(beanDefinition)) {
					addBeanDefinitionToList(beanName, beanDefinition, cachedBeanDefinitions, "gatewaySenders");
				}
			});

		return cachedBeanDefinitions;
	}

	/**
	 * Determines if a {@link BeanDefinition} is of type {@link PeerRegionFactoryBean}, which means it is
	 * effectively of type {@link Region}.
	 *
	 * @return {@literal boolean} indicating whether the Spring bean represents a {@link Region}.
	 */
	private boolean isRegionBean(BeanDefinition beanDefinition, ConfigurableListableBeanFactory beanFactory) {

		return Optional.ofNullable(beanDefinition)
			.flatMap(it -> resolveBeanClass(beanDefinition, beanFactory))
			.filter(PeerRegionFactoryBean.class::isAssignableFrom)
			.isPresent();
	}

	private boolean isGatewaySenderFactoryBean(BeanDefinition beanDefinition) {
		return GatewaySenderFactoryBean.class.getName().equals(beanDefinition.getBeanClassName());
	}

	/**
	 * Creates a {@link Map} of {@literal <<String>, List<BeanDefinitions>>} grouped by the {@literal key}.
	 */
	private Map<String, Map<String, BeanDefinition>> addBeanDefinitionToList(String beanName,
			BeanDefinition beanDefinition, Map<String, Map<String, BeanDefinition>> cachedBeanDefinitions, String key) {

		Map<String, BeanDefinition> beanDefinitions = cachedBeanDefinitions.computeIfAbsent(key, k -> new HashMap<>());

		beanDefinitions.put(beanName, beanDefinition);

		return cachedBeanDefinitions;
	}

	/**
	 * Mapping of {@link GatewaySender} to {@link Region Regions}.
	 */
	private Map<String, List<String>> groupGatewaySenderPerRegion(
			Map<String, Map<String, BeanDefinition>> cachedBeanDefinitions,
			Map<String, List<String>> gatewaySendersPerRegion) {

		Optional<Map<String, BeanDefinition>> gatewaySenders =
			Optional.ofNullable(cachedBeanDefinitions.get("gatewaySenders"));

		gatewaySenders.ifPresent(gatewaySendersMap -> gatewaySendersMap.forEach((key, gatewaySenderBean) -> {

			PropertyValue regions = gatewaySenderBean.getPropertyValues()
				.getPropertyValue("regions");

			List<String> namedRegions = new ArrayList<>();

			Optional.ofNullable(regions)
				.map(PropertyValue::getValue)
				.filter(String[].class::isInstance)
				.map(String[].class::cast)
				.map(Arrays::asList)
				.ifPresent(namedRegions::addAll);

			Collection<String> regionNames = !namedRegions.isEmpty()
				? namedRegions // Add GatewaySender to named Regions
				: cachedBeanDefinitions.get("regions").keySet(); // Add GatewaySender to all Regions

			addGatewaySendersToRegion(gatewaySendersPerRegion, key, regionNames);
		}));

		return gatewaySendersPerRegion;
	}

	private void addGatewaySendersToRegionFactory(ConfigurableListableBeanFactory beanFactory,
			Map<String, List<String>> gatewaySenderToRegions) {

		gatewaySenderToRegions.forEach((key, value) -> {

			ManagedList<RuntimeBeanReference> runtimeBeanReferences = value.stream()
				.map(RuntimeBeanReference::new)
				.collect(Collectors.toCollection(() -> new ManagedList<>()));

			Optional.ofNullable(beanFactory.getBeanDefinition(key)).ifPresent(regionBeanDefinition ->
				regionBeanDefinition.getPropertyValues()
					.addPropertyValue("gatewaySenders", runtimeBeanReferences));
		});
	}

	/**
	 * Mapping of gatewaySenders to individual regions. M -> N mapping capability
	 * @param regionMapping A map that holds the Region -> GatewaySender mapping
	 * @param gatewaySenderName The GatewaySender that is to be mapped to the regions
	 * @param regionNames Collection of defined regions that need to have gatewaySenders mapped to them.
	 */
	private void addGatewaySendersToRegion(Map<String, List<String>> regionMapping, String gatewaySenderName,
			Collection<String> regionNames) {

		regionNames.forEach(regionName -> {

			List<String> gatewaySenders = regionMapping.computeIfAbsent(regionName, k -> new ArrayList<>());

			gatewaySenders.add(gatewaySenderName);
		});
	}
}
