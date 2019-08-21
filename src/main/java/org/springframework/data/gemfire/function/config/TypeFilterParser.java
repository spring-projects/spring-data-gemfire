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
package org.springframework.data.gemfire.function.config;

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.lang.annotation.Annotation;
import java.util.Collection;
import java.util.HashSet;
import java.util.Optional;
import java.util.regex.Pattern;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.FatalBeanException;
import org.springframework.beans.factory.parsing.ReaderContext;
import org.springframework.beans.factory.xml.XmlReaderContext;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.io.ResourceLoader;
import org.springframework.core.type.filter.AnnotationTypeFilter;
import org.springframework.core.type.filter.AspectJTypeFilter;
import org.springframework.core.type.filter.AssignableTypeFilter;
import org.springframework.core.type.filter.RegexPatternTypeFilter;
import org.springframework.core.type.filter.TypeFilter;
import org.springframework.util.Assert;

/**
<<<<<<< Updated upstream
 * Parser to populate the given {@link ClassPathScanningCandidateComponentProvider} with {@link TypeFilter}s parsed from
 * the given {@link Element}'s children.
=======
 * Parser to populate the given {@link ClassPathScanningCandidateComponentProvider} with {@link TypeFilter}s
 * parsed from the given {@link Element}'s children.
>>>>>>> Stashed changes
 *
 * @author Oliver Gierke
 */
class TypeFilterParser {

	private static final String FILTER_EXPRESSION_ATTRIBUTE = "expression";
	private static final String FILTER_TYPE_ATTRIBUTE = "type";

	private final ClassLoader classLoader;

	private final ReaderContext readerContext;

	/**
	 * Creates a new {@link TypeFilterParser} with the given {@link ReaderContext}.
	 *
	 * @param readerContext must not be {@literal null}.
	 * @see org.springframework.beans.factory.xml.XmlReaderContext
	 */
	public TypeFilterParser(XmlReaderContext readerContext) {

		this(readerContext, Optional.ofNullable(readerContext)
			.map(XmlReaderContext::getResourceLoader)
			.map(ResourceLoader::getClassLoader)
			.orElseGet(() -> Thread.currentThread().getContextClassLoader()));
	}

	/**
	 * Constructor to ease testing as {@link XmlReaderContext#getBeanClassLoader()} is final and thus cannot be mocked
	 * easily.
	 *
	 * @param readerContext must not be {@literal null}.
	 * @param classLoader must not be {@literal null}.
	 */
	TypeFilterParser(ReaderContext readerContext, ClassLoader classLoader) {

		Assert.notNull(readerContext, "ReaderContext must not be null!");
		Assert.notNull(classLoader, "ClassLoader must not be null!");

		this.readerContext = readerContext;
		this.classLoader = classLoader;
	}

	public Iterable<TypeFilter> parseTypeFilters(Element element, Type type) {

		Collection<TypeFilter> filters = new HashSet<>();

		NodeList nodeList = element.getChildNodes();

		for (int i = 0; i < nodeList.getLength(); i++) {

			Element childElement = type.getElement(nodeList.item(i));

			if (childElement != null) {
				try {
					filters.add(createTypeFilter(childElement, this.classLoader));
				}
				catch (RuntimeException cause) {
					this.readerContext.error(cause.getMessage(), this.readerContext.extractSource(element),
						cause.getCause());
				}
			}
		}

		return filters;
	}

	protected TypeFilter createTypeFilter(Element element, ClassLoader classLoader) {

		String expression = element.getAttribute(FILTER_EXPRESSION_ATTRIBUTE);
		String filterType = element.getAttribute(FILTER_TYPE_ATTRIBUTE);

		try {

			FilterType filter = FilterType.fromString(filterType);

			return filter.getFilter(expression, classLoader);

		} catch (ClassNotFoundException cause) {
			throw new FatalBeanException("TypeFilter class not found: " + expression, cause);
		}
	}

	/**
	 * Enum representing all the filter types available for {@code include} and {@code exclude} elements. This acts as
	 * factory for {@link TypeFilter} instances.
	 *
	 * @author Oliver Gierke
	 * @see #getFilter(String, ClassLoader)
	 */
	private enum FilterType {

		ANNOTATION {

			@Override
			@SuppressWarnings("unchecked")
			public TypeFilter getFilter(String expression, ClassLoader classLoader) throws ClassNotFoundException {
				return new AnnotationTypeFilter((Class<Annotation>) classLoader.loadClass(expression));
			}
		},

		ASSIGNABLE {

			@Override
			public TypeFilter getFilter(String expression, ClassLoader classLoader) throws ClassNotFoundException {
				return new AssignableTypeFilter(classLoader.loadClass(expression));
			}

		},

		ASPECTJ {

			@Override
			public TypeFilter getFilter(String expression, ClassLoader classLoader) {
				return new AspectJTypeFilter(expression, classLoader);
			}
		},

		REGEX {

			@Override
			public TypeFilter getFilter(String expression, ClassLoader classLoader) {
				return new RegexPatternTypeFilter(Pattern.compile(expression));
			}

		},

		CUSTOM {

			@Override
			public TypeFilter getFilter(String expression, ClassLoader classLoader) throws ClassNotFoundException {

				Class<?> filterClass = classLoader.loadClass(expression);

				if (!TypeFilter.class.isAssignableFrom(filterClass)) {
					throw newIllegalArgumentException("Class is not assignable to [%s]: %s",
						TypeFilter.class.getName(), expression);
				}

				return (TypeFilter) BeanUtils.instantiateClass(filterClass);
			}
		};

		/**
		 * Returns the {@link TypeFilter} for the given expression and {@link ClassLoader}.
		 */
		abstract TypeFilter getFilter(String expression, ClassLoader classLoader) throws ClassNotFoundException;

		/**
		 * Returns the {@link FilterType} for the given type as {@link String}.
		 *
		 * @param type {@link String} containing the name of the type.
		 * @return {@link FilterType} for the given {@link String type name}.
		 * @throws IllegalArgumentException if no {@link FilterType} could be found for the given argument.
		 */
		static FilterType fromString(String type) {

			for (FilterType filter : FilterType.values()) {
				if (filter.name().equalsIgnoreCase(type)) {
					return filter;
				}
			}

			throw new IllegalArgumentException("Unsupported filter type: " + type);
		}
	}

	enum Type {

		INCLUDE("include-filter"),
		EXCLUDE("exclude-filter");

		private final String elementName;

		Type(String elementName) {
			this.elementName = elementName;
		}

		/**
		 * Returns the {@link Element} if the given {@link Node} is an {@link Element} and it's name equals
		 * the one of the type.
		 */
		Element getElement(Node node) {

			if (node.getNodeType() == Node.ELEMENT_NODE) {

				String localName = node.getLocalName();

				if (this.elementName.equals(localName)) {
					return (Element) node;
				}
			}

			return null;
		}
	}
}
