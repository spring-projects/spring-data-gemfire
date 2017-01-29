/*
 * Copyright 2016-2018 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.serialization.json;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.query.SelectResults;
import org.apache.geode.cache.query.internal.ResultsBag;
import org.apache.geode.pdx.JSONFormatter;
import org.apache.geode.pdx.PdxInstance;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.util.CollectionUtils;

/**
 * Spring/AspectJ AOP Aspect to adapt a GemFire {@link Region} to handle JSON data.
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

	private List<String> includedRegions;

	protected final Log log = LogFactory.getLog(JSONRegionAdvice.class);

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
		this.includedRegions = new ArrayList<String>();
		for (Region<?, ?> region : regions) {
			includedRegions.add(region.getName());
		}
	}

	/**
	 * Sets names of regions to be included for JSON conversion. By default, all regions will be included
	 *
	 * @param regionNames a List of region names to include
	 */
	public void setIncludedRegionNames(List<String> regionNames) {
		this.includedRegions = regionNames;
	}

	/**
	 * Flag to print JSON Strings with proper indentation, etc.
	 *
	 * @param prettyPrint false be default
	 */
	public void setPrettyPrint(boolean prettyPrint) {
		this.prettyPrint = prettyPrint;
	}

	@Around("execution(* org.apache.geode.cache.Region.put(..)) || "
		+ "execution(* org.apache.geode.cache.Region.create(..)) ||"
		+ "execution(* org.apache.geode.cache.Region.putIfAbsent(..)) ||"
		+ "execution(* org.apache.geode.cache.Region.replace(..))")
	public Object put(ProceedingJoinPoint pjp) {
		boolean JSONRegion = isIncludedSONRegion(pjp.getTarget());
		Object returnValue = null;

		try {
			if (JSONRegion) {
				Object[] newArgs = Arrays.copyOf(pjp.getArgs(), pjp.getArgs().length);
				Object val = newArgs[1];
				newArgs[1] = convertArgumentToPdxInstance(val);
				returnValue = pjp.proceed(newArgs);
				log.debug("converting " + returnValue + " to JSON string");
				returnValue = convertPdxInstanceToJSONString(returnValue);
			}
			else {
				returnValue = pjp.proceed();
			}
		}
		catch (Throwable t) {
			handleThrowable(t);
		}

		return returnValue;
	}

	@Around("execution(* org.apache.geode.cache.Region.putAll(..))")
	public Object putAll(ProceedingJoinPoint pjp) {
		boolean JSONRegion = isIncludedSONRegion(pjp.getTarget());
		Object returnValue = null;

		try {
			if (JSONRegion) {
				Object[] newArgs = Arrays.copyOf(pjp.getArgs(), pjp.getArgs().length);
				Map<?, ?> val = (Map<?, ?>) newArgs[0];
				Map<Object, Object> newArg = new HashMap<Object, Object>();
				for (Entry<?, ?> entry : val.entrySet()) {
					newArg.put(entry.getKey(), convertArgumentToPdxInstance(entry.getValue()));
				}
				newArgs[0] = newArg;
				returnValue = pjp.proceed(newArgs);
			}
			else {
				returnValue = pjp.proceed();
			}
		}
		catch (Throwable t) {
			handleThrowable(t);
		}

		return returnValue;
	}

	@Around("execution(* org.apache.geode.cache.Region.get(..)) "
		+ "|| execution(* org.apache.geode.cache.Region.selectValue(..))"
		+ "|| execution(* org.apache.geode.cache.Region.remove(..))")
	public Object get(ProceedingJoinPoint pjp) {
		Object returnValue = null;

		try {
			if (isIncludedSONRegion(pjp.getTarget())) {
				returnValue = pjp.proceed();
				log.debug("converting " + returnValue + " to JSON string");
				returnValue = convertPdxInstanceToJSONString(returnValue);
			}
			else {
				returnValue = pjp.proceed();
			}
		}
		catch (Throwable t) {
			handleThrowable(t);
		}

		return returnValue;
	}

	@SuppressWarnings("unchecked")
	@Around("execution(* org.apache.geode.cache.Region.getAll(..))")
	public Map<Object, Object> getAll(ProceedingJoinPoint pjp) {
		Map<Object, Object> result = null;

		try {
			Map<Object, Object> retVal = (Map<Object, Object>) pjp.proceed();
			if (!convertReturnedCollections || CollectionUtils.isEmpty(retVal) || !isIncludedSONRegion(
				pjp.getTarget())) {
				result = retVal;
			}
			else {
				result = new HashMap<Object, Object>();
				for (Entry<Object, Object> entry : retVal.entrySet()) {
					result.put(entry.getKey(), convertPdxInstanceToJSONString(entry.getValue()));
				}
			}
		}
		catch (Throwable t) {
			handleThrowable(t);
		}

		return result;
	}

	@SuppressWarnings("unchecked")
	@Around("execution(* org.apache.geode.cache.Region.values(..))")
	public Collection<Object> values(ProceedingJoinPoint pjp) {
		Collection<Object> result = null;

		try {
			Collection<Object> retVal = (Collection<Object>) pjp.proceed();
			if (!convertReturnedCollections || CollectionUtils.isEmpty(retVal) || !isIncludedSONRegion(
				pjp.getTarget())) {
				result = retVal;
			}
			else {
				result = new ArrayList<Object>();
				for (Object obj : retVal) {
					result.add(convertArgumentToPdxInstance(obj));
				}
			}
		}
		catch (Throwable t) {
			handleThrowable(t);
		}

		return result;
	}

	@Around("execution(* org.springframework.data.gemfire.GemfireOperations.find(..)) " +
		"|| execution(* org.springframework.data.gemfire.GemfireOperations.findUnique(..)) " +
		"|| execution(* org.springframework.data.gemfire.GemfireOperations.query(..))")
	public Object templateQuery(ProceedingJoinPoint pjp) {
		GemfireTemplate template = (GemfireTemplate) pjp.getTarget();
		boolean jsonRegion = isIncludedSONRegion(template.getRegion());
		Object returnValue = null;

		try {
			if (jsonRegion) {
				returnValue = pjp.proceed();
				if (returnValue instanceof SelectResults && convertReturnedCollections) {
					ResultsBag resultsBag = new ResultsBag();
					for (Object obj : (SelectResults<?>) returnValue) {
						resultsBag.add(convertPdxInstanceToJSONString(obj));
					}
					returnValue = resultsBag;
				}
				else {
					returnValue = convertPdxInstanceToJSONString(returnValue);
				}
			}
			else {
				returnValue = pjp.proceed();
			}
		}
		catch (Throwable t) {
			handleThrowable(t);
		}
		return returnValue;
	}


	private PdxInstance convertArgumentToPdxInstance(Object value) {
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
			catch (Throwable t) {
				handleThrowable(t);
			}
		}

		return pdx;
	}

	private boolean isIncludedSONRegion(Object target) {
		Region<?, ?> region = (Region<?, ?>) target;
		boolean result = false;

		if (includedRegions == null || includedRegions.contains(region.getName())) {
			if (log.isDebugEnabled()) {
				log.debug(region.getName() + " is included for JSON conversion");
			}
			result = true;
		}

		return result;
	}

	private Object convertPdxInstanceToJSONString(Object returnValue) {
		Object result = returnValue;

		if (returnValue != null && returnValue instanceof PdxInstance) {
			result = JSONFormatter.toJSON((PdxInstance) returnValue);
			if (!prettyPrint) {
				result = flattenString(result);
			}
		}

		return result;
	}

	private Object flattenString(Object result) {
		if (result instanceof String) {
			String json = (String) result;
			return json.replaceAll("\\s*", "");
		}

		return result;
	}

	private void handleThrowable(Throwable t) {
		if (t instanceof RuntimeException) {
			throw (RuntimeException) t;
		}
		else {
			throw new RuntimeException(t);
		}

	}
}
