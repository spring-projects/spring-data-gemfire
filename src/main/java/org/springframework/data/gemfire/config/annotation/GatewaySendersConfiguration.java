package org.springframework.data.gemfire.config.annotation;

import java.lang.annotation.Annotation;
import java.util.Map;

import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;

/**
 * Spring {@link Configuration} class used to construct, configure and initialize {@link org.apache.geode.cache.wan.GatewaySender} instances
 * in a Spring application context.
 *
 * @author Udo Kohlmeyer
 * @author John Blum
 * @see EnableGatewaySender
 * @see EnableGatewaySenders
 * @see org.apache.geode.cache.wan.GatewayReceiver
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @since 2.2.0
 */
@Configuration
public class GatewaySendersConfiguration extends GatewaySenderConfiguration {

	/**
	 * @param annotationMetadata
	 * @param beanDefinitionRegistry
	 */
	@Override
	public void registerBeanDefinitions(AnnotationMetadata annotationMetadata,
		BeanDefinitionRegistry beanDefinitionRegistry) {

		if (annotationMetadata.hasAnnotation(EnableGatewaySenders.class.getName())) {
			Map<String, Object> annotationAttributesMap = annotationMetadata
				.getAnnotationAttributes(EnableGatewaySenders.class.getName());

			AnnotationAttributes annotationAttributes = AnnotationAttributes.fromMap(annotationAttributesMap);

			registerGatewaySenders(annotationAttributes, beanDefinitionRegistry);
		}
	}

	/**
	 * @param gatewaySendersAnnotation
	 * @param registry
	 */
	private void registerGatewaySenders(AnnotationAttributes gatewaySendersAnnotation,
		BeanDefinitionRegistry registry) {

		AnnotationAttributes[] gatewaySenders = gatewaySendersAnnotation.getAnnotationArray("gatewaySenders");
		if (gatewaySenders.length == 0) {
			registerDefaultGatewaySender(registry, gatewaySendersAnnotation);
		}
		else {
			for (AnnotationAttributes gatewaySender : gatewaySenders) {
				registerGatewaySender(gatewaySender, registry, gatewaySendersAnnotation);
			}
		}
	}


	@Override protected Class<? extends Annotation> getAnnotationType() {
		return EnableGatewaySenders.class;
	}

	private void registerDefaultGatewaySender(BeanDefinitionRegistry registry,
		AnnotationAttributes parentGatewaySendersAnnotation) {
		registerGatewaySender("GatewaySender", parentGatewaySendersAnnotation, registry,
			parentGatewaySendersAnnotation);

	}
}
