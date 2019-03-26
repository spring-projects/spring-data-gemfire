package org.springframework.data.gemfire.config.support;

import static java.util.Arrays.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.BeansException;
import org.springframework.beans.PropertyValue;
import org.springframework.beans.factory.annotation.AnnotatedBeanDefinition;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.type.MethodMetadata;
import org.springframework.data.gemfire.PeerRegionFactoryBean;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.data.gemfire.wan.GatewaySenderFactoryBean;
import org.springframework.util.StringUtils;

/**
 * A {@link BeanFactoryPostProcessor} to associate the configured {@link org.apache.geode.cache.wan.GatewaySender}
 * onto the corresponding {@link org.apache.geode.cache.Region}.
 *
 * @author Udo Kohlmeyer
 * @since 2.2.0
 */
@Configuration
public class GatewaySenderBeanFactoryPostProcessor {

	/**
	 * BeanFactory PostProcessor to map {@link GatewaySenderFactoryBean} to {@link org.apache.geode.cache.Region}
	 *
	 * @return BeanFactoryPostProcessor for the GatewaySenderFactoryBean
	 * @throws BeansException
	 */
	@Bean
	public BeanFactoryPostProcessor postProcessBeanFactory() throws BeansException {
		return beanFactory -> {
			//Create a map of cached BeanDefinitions. Mapped under 'regions' and 'gatewaySenders' for easier lookups
			Map<String, Map<String, BeanDefinition>> cachedBeanDefinitions = populateBeanDefinitionCache(beanFactory);

			//Create a list of gatewaySender to Regions mapping
			Map<String, List<String>> gatewaySenderToRegions = groupGatewaySenderPerRegion(cachedBeanDefinitions,
				new HashMap<>());

			//Add
			addGatewaySendersToRegionFactory(beanFactory, gatewaySenderToRegions);
		};
	}

	/**
	 * Determines if a {@link BeanDefinition} is of type {@link PeerRegionFactoryBean}, which means it is
	 * effectively of type {@link org.apache.geode.cache.Region}
	 * @param beanDefinition
	 * @param beanFactory
	 * @return {@literal boolean}
	 */
	private boolean isRegionBean(BeanDefinition beanDefinition, ConfigurableListableBeanFactory beanFactory) {

		return Optional.ofNullable(beanDefinition)
			.map(it -> resolveBeanClass(beanDefinition, beanFactory))
			.filter(beanClass -> beanClass.isPresent())
			.filter(beanClass -> PeerRegionFactoryBean.class.isAssignableFrom(beanClass.get()))
			.isPresent();
	}

	private void addGatewaySendersToRegionFactory(ConfigurableListableBeanFactory beanFactory,
		Map<String, List<String>> gatewaySenderToRegions) {
		gatewaySenderToRegions.entrySet().forEach(entry -> {
			List<RuntimeBeanReference> beanReferenceList = entry.getValue().stream()
				.map(RuntimeBeanReference::new).collect(Collectors.toList());
			ManagedList<RuntimeBeanReference> runtimeBeanReferences = new ManagedList<>();
			runtimeBeanReferences.addAll(beanReferenceList);
			Optional.ofNullable(beanFactory.getBeanDefinition(entry.getKey())).ifPresent(regionBeanDefinition ->
				regionBeanDefinition.getPropertyValues().addPropertyValue("gatewaySenders", runtimeBeanReferences));
		});
	}

	/**
	 * Mapping of GatewaySender to Regions.
	 * @param cachedBeanDefinitions
	 * @param gatewaySendersPerRegion
	 * @return
	 */
	private Map<String, List<String>> groupGatewaySenderPerRegion(
		Map<String, Map<String, BeanDefinition>> cachedBeanDefinitions,
		Map<String, List<String>> gatewaySendersPerRegion) {

		Optional<Map<String, BeanDefinition>> gatewaySenders = Optional
			.ofNullable(cachedBeanDefinitions.get("gatewaySenders"));

		gatewaySenders.ifPresent(gatewaySendersMap -> gatewaySendersMap.forEach((key, gatewaySenderEntryValue) -> {

			PropertyValue regions = gatewaySenderEntryValue.getPropertyValues().getPropertyValue("regions");
			Optional<PropertyValue> regionsOptional = Optional.ofNullable(regions);

			Collection<String> regionNames;
			List<String> namedRegions = new ArrayList<>();
			regionsOptional.ifPresent(valueHolder -> namedRegions.addAll(Arrays.asList((String[]) valueHolder.getValue())));

			if (namedRegions.size() == 0) {
				//Add gatewaySender to all Regions
				regionNames = cachedBeanDefinitions.get("regions").keySet();
			}
			else {
				//Add gatewaySender to named Regions
				regionNames = namedRegions;
			}
			addGatewaySendersToRegion(gatewaySendersPerRegion, key,
				regionNames);
		}));

		return gatewaySendersPerRegion;
	}

	/**
	 * Caches BeanDefinitions for GatewaySenders and Regions. Maps the beandefinitions under the keys of
	 * `regions` and `gatewaySenders`
	 * @param beanFactory
	 * @return Map containing all BeanDefinitions for Regions and GatewaySenders
	 */
	private Map<String, Map<String, BeanDefinition>> populateBeanDefinitionCache(
		ConfigurableListableBeanFactory beanFactory) {
		Map<String, Map<String, BeanDefinition>> cachedBeanDefinitions = new LinkedHashMap<>();
		stream(nullSafeArray(beanFactory.getBeanDefinitionNames(), String.class))
			.forEach(beanName -> Optional.of(beanFactory.getBeanDefinition(beanName))
				.ifPresent(beanDefinition -> {
					if (isRegionBean(beanDefinition, beanFactory)) {
						addBeanDefinitionToList(beanName, beanDefinition, cachedBeanDefinitions, "regions");
					}
					else if (isGatewaySenderFactoryBean(beanDefinition)) {
						addBeanDefinitionToList(beanName, beanDefinition, cachedBeanDefinitions, "gatewaySenders");
					}
				}));
		return cachedBeanDefinitions;
	}

	/**
	 * Creates a map of <<String>,List<BeanDefinitions>> grouped by the `key`
	 *
	 * @param beanName
	 * @param beanDefinition
	 * @param cachedBeanDefinitions
	 * @param key
	 * @return
	 */
	private Map<String, Map<String, BeanDefinition>> addBeanDefinitionToList(String beanName,
		BeanDefinition beanDefinition,
		Map<String, Map<String, BeanDefinition>> cachedBeanDefinitions, String key) {
		Map<String, BeanDefinition> beanDefinitions = cachedBeanDefinitions.get(key);
		if (beanDefinitions == null) {
			beanDefinitions = new HashMap<>();
		}
		beanDefinitions.put(beanName, beanDefinition);
		cachedBeanDefinitions.put(key, beanDefinitions);
		return cachedBeanDefinitions;
	}

	/**
	 * Mapping of gatewaySenders to individual regions. M -> N mapping capability
	 * @param regionMapping A map that holds the Region -> GatewaySender mapping
	 * @param gatewaySenderName The GatewaySender that is to be mapped to the regions
	 * @param regionNames Collection of defined regions that need to have gatewaySenders mapped to them.
	 */
	private void addGatewaySendersToRegion(Map<String, List<String>> regionMapping,
		String gatewaySenderName, Collection<String> regionNames) {
		regionNames.forEach(regionName -> {
			List<String> gatewaySenders = regionMapping.get(regionName);
			if (gatewaySenders == null) {
				gatewaySenders = new ArrayList<>();
			}
			gatewaySenders.add(gatewaySenderName);
			regionMapping.put(regionName, gatewaySenders);
		});
	}

	private boolean isGatewaySenderFactoryBean(BeanDefinition beanDefinition) {
		return GatewaySenderFactoryBean.class.getName()
			.equals(beanDefinition.getBeanClassName());
	}

	protected Optional<Object> getPropertyValue(BeanDefinition beanDefinition, String propertyName) {
		return SpringUtils.getPropertyValue(beanDefinition, propertyName);
	}

	/**
	 * Resolves the class type name of the bean defined by the given {@link BeanDefinition}.
	 *
	 * @param beanDefinition {@link BeanDefinition} defining the bean from which to resolve the class type name.
	 * @return an {@link Optional} {@link String} containing the resolved class type name of the bean defined
	 * by the given {@link BeanDefinition}.
	 * @see org.springframework.beans.factory.config.BeanDefinition#getBeanClassName()
	 */
	protected Optional<Class> resolveBeanClass(BeanDefinition beanDefinition,
		ConfigurableListableBeanFactory beanFactory) {

		Optional<String> beanClassName = Optional.ofNullable(beanDefinition)
			.map(BeanDefinition::getBeanClassName)
			.filter(StringUtils::hasText);

		if (!beanClassName.isPresent()) {
			beanClassName = Optional.ofNullable(beanDefinition)
				.filter(it -> it instanceof AnnotatedBeanDefinition)
				.filter(it -> StringUtils.hasText(it.getFactoryMethodName()))
				.map(it -> ((AnnotatedBeanDefinition) it).getFactoryMethodMetadata())
				.map(MethodMetadata::getReturnTypeName);
		}

		return beanClassName.map(className -> {
			try {
				return beanFactory.getBeanClassLoader().loadClass(className);
			}
			catch (ClassNotFoundException e) {
				e.printStackTrace();
			}
			return null;
		});
	}
}
