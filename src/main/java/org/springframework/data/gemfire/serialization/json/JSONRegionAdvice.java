/*
 * Copyright 2016-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.serialization.json;

import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeList;
import static org.springframework.data.gemfire.util.RegionUtils.toRegionName;
import static org.springframework.data.gemfire.util.RegionUtils.toRegionPath;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.query.SelectResults;
import org.apache.geode.cache.query.internal.ResultsBag;
import org.apache.geode.pdx.JSONFormatter;
import org.apache.geode.pdx.PdxInstance;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.util.CollectionUtils;

/**
 * Spring/AspectJ AOP Aspect adapting a {@link Region} to handle JSON data.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.pdx.JSONFormatter
 * @see org.apache.geode.pdx.PdxInstance
 * @see org.aspectj.lang.annotation.Aspect
 * @see org.aspectj.lang.annotation.Around
 */
@Aspect
@SuppressWarnings("unused")
public class JSONRegionAdvice {

	private boolean convertReturnedCollections = true;
	private boolean prettyPrint = false;

	private List<String> includedRegions = new ArrayList<>();

	protected final Logger logger = LoggerFactory.getLogger(JSONRegionAdvice.class);

	/**
	 * Flag to convert collections returned from cache from @{link PdxInstance} to JSON String. If the returned
	 * collections are very large, overhead will be incurred to covert all the values from from
	 * Region.getAll() and Region.values()
	 *
	 * @param convertReturnedCollections true by default
	 */
	public void setConvertReturnedCollections(boolean convertReturnedCollections) {
		this.convertReturnedCollections = convertReturnedCollections;
	}

	/**
	 * Sets regions to be included for JSON conversion. By default, all regions will be included
	 *
	 * @param regions a List of region names to include
	 */
	public void setIncludedRegions(List<Region<?, ?>> regions) {
		nullSafeList(regions).forEach(region -> this.includedRegions.add(toRegionName(region)));
	}

	/**
	 * Sets names of regions to be included for JSON conversion. By default, all regions will be included
	 *
	 * @param regionNames a List of region names to include
	 */
	public void setIncludedRegionNames(List<String> regionNames) {
		this.includedRegions = nullSafeList(regionNames);
	}

	/**
	 * Flag to print JSON Strings with proper indentation, etc.
	 *
	 * @param prettyPrint false be default
	 */
	public void setPrettyPrint(boolean prettyPrint) {
		this.prettyPrint = prettyPrint;
	}

	@Around("execution(* org.apache.geode.cache.Region.get(..))"
		+ " || execution(* org.apache.geode.cache.Region.remove(..))"
		+ " || execution(* org.apache.geode.cache.Region.selectValue(..))")
	public Object get(ProceedingJoinPoint pjp) {

		Object returnValue = null;

		try {
			if (isIncludedJsonRegion(pjp.getTarget())) {
				returnValue = pjp.proceed();
				logger.debug("converting {} to JSON string", returnValue);
				returnValue = convertToJson(returnValue);
			}
			else {
				returnValue = pjp.proceed();
			}
		}
		catch (Throwable cause) {
			handleThrowable(cause);
		}

		return returnValue;
	}

	@SuppressWarnings("unchecked")
	@Around("execution(* org.apache.geode.cache.Region.getAll(..))")
	public Map<Object, Object> getAll(ProceedingJoinPoint pjp) {

		Map<Object, Object> result = null;

		try {

			Map<Object, Object> returnValue = (Map<Object, Object>) pjp.proceed();

			if (!this.convertReturnedCollections || CollectionUtils.isEmpty(returnValue)
				|| !isIncludedJsonRegion(pjp.getTarget())) {

				result = returnValue;
			}
			else {
				result = returnValue.entrySet().stream()
					.collect(Collectors.toMap(Map.Entry::getKey, entry -> convertToJson(entry.getValue())));
			}
		}
		catch (Throwable t) {
			handleThrowable(t);
		}

		return result;
	}

	@Around("execution(* org.apache.geode.cache.Region.create(..))"
		+ " || execution(* org.apache.geode.cache.Region.put(..))"
		+ " || execution(* org.apache.geode.cache.Region.putIfAbsent(..))"
		+ " || execution(* org.apache.geode.cache.Region.replace(..))")
	public Object put(ProceedingJoinPoint pjp) {

		Object returnValue = null;

		try {
			if (isIncludedJsonRegion(pjp.getTarget())) {

				Object[] newArgs = Arrays.copyOf(pjp.getArgs(), pjp.getArgs().length);
				Object val = newArgs[1];

				newArgs[1] = convertToPdx(val);
				returnValue = pjp.proceed(newArgs);
				logger.debug("Converting [{}] to JSON", returnValue);
				returnValue = convertToJson(returnValue);
			}
			else {
				returnValue = pjp.proceed();
			}
		}
		catch (Throwable cause) {
			handleThrowable(cause);
		}

		return returnValue;
	}

	@Around("execution(* org.apache.geode.cache.Region.putAll(..))")
	public Object putAll(ProceedingJoinPoint pjp) {

		Object returnValue = null;

		try {
			if (isIncludedJsonRegion(pjp.getTarget())) {

				Object[] newArgs = Arrays.copyOf(pjp.getArgs(), pjp.getArgs().length);

				Map<?, ?> map = (Map<?, ?>) newArgs[0];
				Map<Object, Object> newArg = new HashMap<>();

				for (Entry<?, ?> entry : map.entrySet()) {
					newArg.put(entry.getKey(), convertToPdx(entry.getValue()));
				}

				newArgs[0] = newArg;
				returnValue = pjp.proceed(newArgs);
			}
			else {
				returnValue = pjp.proceed();
			}
		}
		catch (Throwable cause) {
			handleThrowable(cause);
		}

		return returnValue;
	}

	@SuppressWarnings("unchecked")
	@Around("execution(* org.apache.geode.cache.Region.values(..))")
	public Collection<Object> values(ProceedingJoinPoint pjp) {

		Collection<Object> result = null;

		try {

			Collection<Object> returnValue = (Collection<Object>) pjp.proceed();

			if (!this.convertReturnedCollections || CollectionUtils.isEmpty(returnValue)
					|| !isIncludedJsonRegion(pjp.getTarget())) {

				result = returnValue;
			}
			else {
				result = returnValue.stream().map(this::convertToPdx).collect(Collectors.toList());
			}
		}
		catch (Throwable cause) {
			handleThrowable(cause);
		}

		return result;
	}

	@Around("execution(* org.springframework.data.gemfire.GemfireOperations.find(..)) " +
		"|| execution(* org.springframework.data.gemfire.GemfireOperations.findUnique(..)) " +
		"|| execution(* org.springframework.data.gemfire.GemfireOperations.query(..))")
	public Object templateQuery(ProceedingJoinPoint pjp) {

		GemfireTemplate template = (GemfireTemplate) pjp.getTarget();

		boolean jsonRegion = isIncludedJsonRegion(template.getRegion());

		Object returnValue = null;

		try {
			if (jsonRegion) {

				returnValue = pjp.proceed();

				if (returnValue instanceof SelectResults && this.convertReturnedCollections) {

					ResultsBag resultsBag = new ResultsBag();

					for (Object obj : (SelectResults<?>) returnValue) {
						resultsBag.add(convertToJson(obj));
					}

					returnValue = resultsBag;
				}
				else {
					returnValue = convertToJson(returnValue);
				}
			}
			else {
				returnValue = pjp.proceed();
			}
		}
		catch (Throwable cause) {
			handleThrowable(cause);
		}
		return returnValue;
	}


	private boolean isIncludedJsonRegion(Object target) {
		return target instanceof Region && isIncludedJsonRegion((Region) target);
	}

	private boolean isIncludedJsonRegion(Region region) {

		boolean result = false;

		if (isIncludedJsonRegion(toRegionName(region), toRegionPath(region))) {

			if (logger.isDebugEnabled()) {
				logger.debug("Region [{}] is included for JSON conversion", region.getName());
			}

			result = true;
		}

		return result;
	}

	private boolean isIncludedJsonRegion(String... regionReferences) {

		List<String> regionReferencesList = Arrays.asList(nullSafeArray(regionReferences, String.class));

		return CollectionUtils.isEmpty(this.includedRegions) || this.includedRegions.stream()
			.anyMatch(includeRegion -> regionReferencesList.contains(includeRegion));
	}

	private Object convertToJson(Object returnValue) {

		Object result = returnValue;

		if (returnValue instanceof PdxInstance) {

			result = JSONFormatter.toJSON((PdxInstance) returnValue);

			if (!this.prettyPrint) {
				result = flattenString(result);
			}
		}

		return result;
	}

	private PdxInstance convertToPdx(Object value) {

		PdxInstance pdx = null;

		if (value instanceof PdxInstance) {
			pdx = (PdxInstance) value;
		}
		else if (value instanceof String) {
			pdx = JSONFormatter.fromJSON((String) value);
		}
		else {

			ObjectMapper mapper = new ObjectMapper();

			try {
				String json = mapper.writeValueAsString(value);
				pdx = JSONFormatter.fromJSON(json);
			}
			catch (Throwable cause) {
				handleThrowable(cause);
			}
		}

		return pdx;
	}

	private Object flattenString(Object result) {
		return result instanceof String ? ((String) result).replaceAll("\\s*", "") : result;
	}

	private void handleThrowable(Throwable cause) {

		if (cause instanceof RuntimeException) {
			throw (RuntimeException) cause;
		}
		else {
			throw new RuntimeException(cause);
		}
	}
}
